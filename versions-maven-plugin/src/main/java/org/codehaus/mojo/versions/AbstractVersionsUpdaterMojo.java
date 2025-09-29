package org.codehaus.mojo.versions;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.rule.RuleService;
import org.codehaus.mojo.versions.rule.RulesServiceBuilder;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.VersionsExpressionEvaluator;
import org.eclipse.aether.RepositorySystem;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Optional.ofNullable;

/**
 * Abstract base class for Versions Mojos.
 *
 * @author Stephen Connolly
 */
public abstract class AbstractVersionsUpdaterMojo extends AbstractMojo {
    /**
     * The Maven Project.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(defaultValue = "${project}", required = true, readonly = true)
    protected MavenProject project;

    /**
     * The (injected) {@link org.eclipse.aether.RepositorySystem} instance.
     */
    protected RepositorySystem repositorySystem;

    /**
     * The reactor projects for the current build.
     * @since 1.0-alpha-1
     */
    @Parameter(defaultValue = "${reactorProjects}", required = true, readonly = true)
    protected List<MavenProject> reactorProjects;

    /**
     * settings.xml's server id for the URL. This is used when wagon needs extra authentication information.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "maven.version.rules.serverId", defaultValue = "serverId")
    protected String serverId;

    /**
     * URI of a ruleSet file containing the rules that control how to compare
     * version numbers. The URI could be either a Wagon URI or a classpath URI
     * (e.g. <code>classpath:///package/sub/package/rules.xml</code>).
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "maven.version.rules")
    protected String rulesUri;

    /**
     * Controls whether a backup pom should be created.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "generateBackupPoms", defaultValue = "true")
    protected boolean generateBackupPoms;

    /**
     * Our versions helper.
     */
    private VersionsHelper helper;

    /**
     * The Maven Session.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(defaultValue = "${session}", required = true, readonly = true)
    protected MavenSession session;

    /**
     * The current mojo execution.
     */
    @Parameter(defaultValue = "${mojoExecution}", required = true, readonly = true)
    protected MojoExecution mojoExecution;

    /**
     * <p>The format used to record changes. The following formats are supported:</p>
     * <ul>
     *     <li>{@code none}: will not output any changes</li>
     *     <li>{@code xml}: outputs changes to an XML format.
     *     <p>If no {@link #changeRecorderOptions} are provided,
     *     or if the {@code legacy} option is not specified, or if {@code legacy=true} is provided as an option,
     *     the "legacy" format conforming to the
     *     {@code "http://www.mojohaus.org/versions-maven-plugin/schema/updates/2.0"} namespace will be selected.</p>
     *     <p>If the options contain {@code legacy=false}, the new renderer is used, conforming to
     *     {@code "http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0"}</p>
     *     </li>
     *     <li>{@code json} outputs changes to a Json file</li>
     *     <li>{@code csv} outputs changes to a CSV file</li>
     * </ul>
     *
     * @since 2.11
     */
    @Parameter(property = "changeRecorderFormat", defaultValue = "none")
    private String changeRecorderFormat = "none";

    /**
     * <p>Optional options to be passed to the selected change recorder.</p>
     * <p>The format of the options is a string consisting of a {@code key=value} pairs divided by colon ({@code :})
     * signs, e.g. {@code key0=value0:key1=value1:key2=value2}.
     *
     * @since 2.20
     */
    @Parameter(property = "changeRecorderOptions")
    private String changeRecorderOptions;

    /**
     * The output file used to record changes.
     * Default value is {@code ${project.build.directory}/versions-changes.<ext>},
     * where {@code <ext>} is provided by the respective format handler.
     *
     * @since 2.11
     * @deprecated use #changeRecorderOutputFileName
     */
    @Parameter(property = "changeRecorderOutputFile")
    @Deprecated
    private File changeRecorderOutputFile;

    /**
     * The output file <em>name</em> used to create a change recorder file within {@code ${project.build.directory}}
     * (which usually resolves to {@code target}).
     * Default value is {@code versions-changes.<ext>},
     * where {@code <ext>} is provided by the respective format handler.
     *
     * @since 2.20
     */
    @Parameter(property = "changeRecorderOutputFileName")
    private String changeRecorderOutputFileName;

    /**
     * <p>Allows specifying the {@linkplain RuleSet} object describing rules
     * on artifact versions to ignore when considering updates.</p>
     *
     * @see <a
     *         href="https://www.mojohaus.org/versions/versions-maven-plugin/version-rules.html#Using_the_ruleSet_element_in_the_POM">
     *         Using the ruleSet element in the POM</a>
     * @since 2.13.0
     */
    @Parameter
    protected RuleSet ruleSet;

    /**
     * <p>Allows specifying ignored versions directly as an alternative
     * to providing the {@linkplain #ruleSet} parameter; mainly created
     * for {@code -D} property usage.</p>
     * <p>
     * Example: {@code "1\.0\.1,.+-M.,.*-SNAPSHOT"}
     * </p>
     * <p><em>Currently, this parameter will override the defined {@link #ruleSet}</em></p>
     *
     * @since 2.13.0
     */
    @Parameter(property = "maven.version.ignore")
    protected Set<String> ignoredVersions;

    /**
     * (injected) map of {@link Wagon} instances per protocol
     *
     * @since 2.14.0
     */
    protected final Map<String, Wagon> wagonMap;

    /**
     * An {@link ArtifactFactory} instance.
     */
    protected final ArtifactFactory artifactFactory;

    private final Map<String, VersionChangeRecorderFactory> changeRecorderFactories;

    private VersionChangeRecorder changeRecorder;

    // --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Constructor used by Mojos that update versions.
     *
     * @param artifactFactory the artifact factory
     * @param repositorySystem the repository system
     * @param wagonMap map of wagon instances
     * @param changeRecorderFactories map of change recorder factories
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    protected AbstractVersionsUpdaterMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        this.artifactFactory = artifactFactory;
        this.repositorySystem = repositorySystem;
        this.wagonMap = wagonMap;
        // the actual factory is only known in the child class
        this.changeRecorderFactories = changeRecorderFactories;
    }

    /**
     * {@code true} if snapshots should be allowed when searching for newer versions of artifacts. This can be
     * overridden on a per-call basis by passing a non-{@code null} value to the
     * {@link #findLatestVersion(Artifact, VersionRange, Boolean, boolean)} method.
     *
     * Default is {@code false} for all Mojos except {@code versions:use-latest-versions} and
     * {@code versions:use-latest-releases} where it is {@code true}
     * @return {@code true} if snapshots should be allowed when searching for newer versions of artifacts.
     * @since 1.0-alpha-1
     * @see #findLatestVersion(Artifact, VersionRange, Boolean, boolean)
     */
    protected abstract boolean getAllowSnapshots();

    /**
     * Returns the configured {@link VersionsHelper} instance, creating it lazily.
     *
     * @return the {@link VersionsHelper}
     * @throws MojoExecutionException if the helper cannot be created
     */
    public synchronized VersionsHelper getHelper() throws MojoExecutionException {
        if (helper == null) {
            RuleService ruleService = new RulesServiceBuilder()
                    .withMavenSession(session)
                    .withWagonMap(wagonMap)
                    .withServerId(serverId)
                    .withRulesUri(rulesUri)
                    .withRuleSet(ruleSet)
                    .withIgnoredVersions(ignoredVersions)
                    .withLog(getLog())
                    .build();
            PomHelper pomHelper =
                    new PomHelper(artifactFactory, new VersionsExpressionEvaluator(session, mojoExecution));
            helper = new DefaultVersionsHelper.Builder()
                    .withArtifactFactory(artifactFactory)
                    .withRepositorySystem(repositorySystem)
                    .withLog(getLog())
                    .withMavenSession(session)
                    .withPomHelper(pomHelper)
                    .withRuleService(ruleService)
                    .build();
        }
        return helper;
    }

    /**
     * Getter for property 'project'.
     *
     * @return Value for property 'project'.
     * @since 1.0-alpha-1
     */
    public MavenProject getProject() {
        return project;
    }

    /**
     * Setter for property 'project'.
     *
     * @param project Value to set for property 'project'.
     * @since 1.0-alpha-1
     */
    public void setProject(MavenProject project) {
        this.project = project;
    }

    /**
     * Gets the version of the project being processed.
     *
     * @return the version of the project being processed or {@code null} if no project is set
     * @since 1.0-alpha-1
     */
    public String getVersion() {
        return getProject() == null ? null : getProject().getVersion();
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * {@inheritDoc}
     *
     * @since 1.0-alpha-1
     */
    public void execute() throws MojoExecutionException, MojoFailureException {
        validateInput();
        File outFile = project.getFile();
        process(outFile);
    }

    // -------------------------- OTHER METHODS --------------------------

    /**
     * Validates input parameters
     *
     * @throws MojoExecutionException thrown if any of input parameters is invalid
     */
    protected void validateInput() throws MojoExecutionException {}

    /**
     * Finds the latest version of the specified artifact that matches the version range.
     *
     * @param artifact              The artifact.
     * @param versionRange          The version range.
     * @param allowingSnapshots     <code>null</code> for no override, otherwise the local override to apply.
     * @param usePluginRepositories Use plugin repositories
     * @return The latest version of the specified artifact that matches the specified version range or
     *         <code>null</code> if no matching version could be found.
     * @throws VersionRetrievalException If the artifact metadata could not be found.
     * @throws MojoExecutionException    if something goes wrong.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion(
            Artifact artifact, VersionRange versionRange, Boolean allowingSnapshots, boolean usePluginRepositories)
            throws MojoExecutionException, VersionRetrievalException {
        boolean includeSnapshots = allowingSnapshots != null ? allowingSnapshots : getAllowSnapshots();
        final ArtifactVersions artifactVersions =
                getHelper().lookupArtifactVersions(artifact, versionRange, usePluginRepositories);
        return artifactVersions.getNewestVersion(versionRange, null, includeSnapshots, false);
    }

    /**
     * Processes the specified file. This is an extension point to allow updating a file external to the reactor.
     *
     * @param outFile The file to process.
     * @throws MojoExecutionException If things go wrong.
     * @throws MojoFailureException   If things go wrong.
     * @since 1.0-alpha-1
     */
    protected void process(File outFile) throws MojoExecutionException, MojoFailureException {
        try {
            MutableXMLStreamReader newPom = new MutableXMLStreamReader(outFile.toPath());

            update(newPom);

            if (newPom.isModified()) {
                if (generateBackupPoms) {
                    File backupFile = new File(outFile.getParentFile(), outFile.getName() + ".versionsBackup");
                    if (!backupFile.exists()) {
                        getLog().debug("Backing up " + outFile + " to " + backupFile);
                        Files.copy(outFile.toPath(), backupFile.toPath(), REPLACE_EXISTING);
                    } else {
                        getLog().debug("Leaving existing backup " + backupFile + " unmodified");
                    }
                } else {
                    getLog().debug("Skipping generation of backup file");
                }
                try (Writer writer = Files.newBufferedWriter(
                        outFile.toPath(),
                        ofNullable(newPom.getEncoding()).map(Charset::forName).orElse(Charset.defaultCharset()))) {
                    writer.write(newPom.getSource());
                }
            }
            saveChangeRecorderResults();
        } catch (IOException e) {
            getLog().error(e);
        } catch (VersionRetrievalException | XMLStreamException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    /**
     * Updates the pom.
     *
     * @param pom The pom to update.
     * @throws MojoExecutionException              If things go wrong.
     * @throws MojoFailureException                If things go wrong.
     * @throws javax.xml.stream.XMLStreamException If things go wrong.
     * @throws VersionRetrievalException           if version retrieval goes wrong
     * @since 1.0-alpha-1
     */
    protected abstract void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException;

    /**
     * @param artifact       The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion  The proposed new version of the artifact.
     * @return <code>true</code> if the update should be applied.
     * @since 1.0-alpha-1
     * @deprecated This method no longer supported.
     *         use shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion, Boolean
     *         forceUpdate )
     *         <p>
     *         Returns <code>true</code> if the update should be applied.
     */
    @Deprecated
    protected boolean shouldApplyUpdate(Artifact artifact, String currentVersion, ArtifactVersion updateVersion) {
        return shouldApplyUpdate(artifact, currentVersion, updateVersion, false);
    }

    /**
     * Returns <code>true</code> if the update should be applied.
     *
     * @param artifact       The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion  The proposed new version of the artifact.
     * @param forceUpdate    if true, LATEST and RELEASE versions will be overwritten with the real version
     * @return <code>true</code> if the update should be applied to the pom.
     * @since 2.9
     */
    protected boolean shouldApplyUpdate(
            Artifact artifact, String currentVersion, ArtifactVersion updateVersion, boolean forceUpdate) {
        getLog().debug("Proposal is to update from " + currentVersion + " to " + updateVersion);

        if (updateVersion == null) {
            getLog().warn("Not updating version: could not resolve any versions");
            return false;
        }

        if (forceUpdate) {
            getLog().info("Force update enabled. LATEST or RELEASE versions will be overwritten with real version");
            return true;
        }

        artifact.setVersion(updateVersion.toString());
        try {
            getHelper().resolveArtifact(artifact, false);
        } catch (ArtifactResolutionException | MojoExecutionException e) {
            getLog().warn("Not updating version: could not resolve " + artifact, e);
            return false;
        }

        if (currentVersion.equals(updateVersion.toString())) {
            getLog().info("Current version of " + artifact + " is the latest.");
            return false;
        }
        return true;
    }

    /**
     * Attempts to update the property to a newer version, if that exists
     *
     * @param pom              pom to update
     * @param property         property to update
     * @param version          {@link PropertyVersions} object
     * @param currentVersion   current version
     * @param allowDowngrade   if downgrades should be allowed if snapshots are not allowed
     * @param unchangedSegment most major segment not to be changed
     * @return new version of the artifact, if the property was updated; {@code null} if there was no update
     * @throws XMLStreamException                   thrown from {@link MutableXMLStreamReader} if the update doesn't
     *                                              succeed
     * @throws InvalidVersionSpecificationException thrown if {@code unchangedSegment} doesn't match the version
     * @throws InvalidSegmentException              thrown if {@code unchangedSegment} is invalid
     */
    protected ArtifactVersion updatePropertyToNewestVersion(
            MutableXMLStreamReader pom,
            Property property,
            PropertyVersions version,
            String currentVersion,
            boolean allowDowngrade,
            Optional<Segment> unchangedSegment)
            throws XMLStreamException, InvalidVersionSpecificationException, InvalidSegmentException {
        ArtifactVersion winner = version.getNewestVersion(
                currentVersion, property, getAllowSnapshots(), this.reactorProjects, allowDowngrade, unchangedSegment);

        if (winner == null || currentVersion.equals(winner.toString())) {
            getLog().info("Property ${" + property.getName() + "}: Leaving unchanged as " + currentVersion);
        } else if (PomHelper.setPropertyVersion(pom, version.getProfileId(), property.getName(), winner.toString())) {
            getLog().info("Updated ${" + property.getName() + "} from " + currentVersion + " to " + winner);
            return winner;
        }

        return null;
    }

    /**
     * Parses the {@link #changeRecorderOptions} into a map of key-value pairs.
     *
     * @return the parsed options or {@code null} if no options were provided
     * @throws IllegalArgumentException if the options are malformed
     */
    protected Map<String, String> getChangeRecorderOptions() {
        if (changeRecorderOptions == null) {
            return null;
        }
        return Arrays.stream(changeRecorderOptions.split(":"))
                .map(option -> {
                    String[] kv = option.split("=");
                    if (kv.length != 2) {
                        throw new IllegalArgumentException("Invalid value of changeRecorderOptions ("
                                + changeRecorderOptions + "): each option must"
                                + " be a key=value pair, but \"" + option + "\" isn't.");
                    }
                    return kv;
                })
                .collect(Collectors.toMap(kv -> kv[0], kv -> kv[1]));
    }

    /**
     * Configure and return the change recorder.
     *
     * @return The change recorder
     */
    protected synchronized VersionChangeRecorder getChangeRecorder() {
        if (changeRecorder == null) {
            VersionChangeRecorderFactory factory = changeRecorderFactories.get(changeRecorderFormat);
            if (factory == null) {
                throw new IllegalStateException(
                        "Only " + changeRecorderFactories.keySet() + " formats are supported for change recordings");
            }
            changeRecorder = factory.create(session, mojoExecution, getLog(), getChangeRecorderOptions());
        }
        return changeRecorder;
    }

    /**
     * Provides the effective change recorder output file name
     *
     * @return effective change recorder output file name
     */
    protected Path getChangeRecorderOutputFile() {
        if (changeRecorderOutputFileName != null) {
            return getProject()
                    .getBasedir()
                    .toPath()
                    .resolve(getProject().getBuild().getDirectory())
                    .resolve(changeRecorderOutputFileName);
        } else if (changeRecorderOutputFile != null) {
            return changeRecorderOutputFile.toPath();
        }
        return getProject()
                .getBasedir()
                .toPath()
                .resolve(getProject().getBuild().getDirectory())
                .resolve(getChangeRecorder().getDefaultFileName());
    }

    /**
     * Save all the changes recorded by the change recorder.
     *
     * @throws IOException           On I/O errors
     * @throws IllegalStateException if the selected change recorder is not supported
     */
    protected void saveChangeRecorderResults() throws IOException {
        Path outputFile = getChangeRecorderOutputFile();
        getChangeRecorder().writeReport(outputFile);
    }
}
