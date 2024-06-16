package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.change.VersionChanger;
import org.codehaus.mojo.versions.change.VersionChangerFactory;
import org.codehaus.mojo.versions.ordering.ReactorDepthComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.ContextualLog;
import org.codehaus.mojo.versions.utils.DelegatingContextualLog;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;

import static org.apache.commons.lang3.StringUtils.isEmpty;

/**
 * Sets the current project's version and based on that change propagates that change onto any child modules as
 * necessary.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo(name = "set", aggregator = true, threadSafe = true)
public class SetMojo extends AbstractVersionsUpdaterMojo {

    private static final String SNAPSHOT = "-SNAPSHOT";

    /**
     * The new version number to set.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "newVersion")
    private String newVersion;

    /**
     * If set to {@code true}, will process all modules regardless whether they
     * match {@code groupId:artifactId:oldVersion}.
     *
     * @since 2.5
     */
    @Parameter(property = "processAllModules", defaultValue = "false")
    private boolean processAllModules;

    /**
     * <p>The <b>non-interpolated</b> groupId of the dependency/module to be selected for update.</p>
     * <p>If not set, will be equal to the non-interpolated groupId of the project file.</p>
     * <p>If you wish to update modules of a aggregator regardless of the groupId, you
     * should set {@code -DgroupId='*'} to ignore the groupId of the current project.</p>
     * <p>Alternatively, you can use {@code -DprocessAllModules=true}</p>
     * <p><u>The goal does not interpolate the properties used in groupId used in the pom.xml file.</u></p>
     * <p><i>The single quotes are only necessary on POSIX-compatible shells (Linux, MacOS, etc.).</i></p>
     *
     * @since 1.2
     */
    @Parameter(property = "groupId")
    private String groupId;

    /**
     * <p>The <b>non-interpolated</b> artifactId of the dependency/module to be selected for update.</p>
     * <p>If not set, will be equal to the non-interpolated artifactId of the project file.</p>
     * <p>If you wish to update modules of a aggregator regardless of the artifactId, you
     * should set {@code -DartifactId='*'} to ignore the artifactId of the current project.</p>
     * <p>Alternatively, you can use {@code -DprocessAllModules=true}</p>
     * <p><u>The goal does not interpolate the properties used in artifactId used in the pom.xml file.</u></p>
     * <p><i>The single quotes are only necessary on POSIX-compatible shells (Linux, MacOS, etc.).</i></p>
     *
     * @since 1.2
     */
    @Parameter(property = "artifactId")
    private String artifactId;

    /**
     * <p>The <b>non-interpolated</b> version of the dependency/module to be selected for update.</p>
     * <p>If not set, will be equal to the non-interpolated version of the project file.</p>
     * <p>If you wish to update modules of a aggregator regardless of the version, you
     * should set {@code -Dversion='*'} to ignore the version of the current project.</p>
     * <p>Alternatively, you can use {@code -DprocessAllModules=true}</p>
     * <p><u>The goal does not interpolate the properties used in version used in the pom.xml file.</u></p>
     * <p><i>The single quotes are only necessary on POSIX-compatible shells (Linux, MacOS, etc.).</i></p>
     *
     * @since 1.2
     */
    @Parameter(property = "oldVersion")
    private String oldVersion;

    /**
     * Whether matching versions explicitly specified (as /project/version) in child modules should be updated.
     *
     * @since 1.3
     */
    @Parameter(property = "updateMatchingVersions", defaultValue = "true")
    private boolean updateMatchingVersions;

    /**
     * Whether to process the parent of the project.
     *
     * @since 1.3
     */
    @Parameter(property = "processParent", defaultValue = "true")
    private boolean processParent;

    /**
     * Whether to process the project version.
     *
     * @since 1.3
     */
    @Parameter(property = "processProject", defaultValue = "true")
    private boolean processProject;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.3
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    private boolean processDependencies;

    /**
     * Whether to process the plugins section of the project.
     *
     * @since 1.3
     */
    @Parameter(property = "processPlugins", defaultValue = "true")
    private boolean processPlugins;

    /**
     * Component used to prompt for input
     */
    private Prompter prompter;

    /**
     * Whether to remove <code>-SNAPSHOT</code> from the existing version.
     *
     * @since 2.10
     */
    @Parameter(property = "removeSnapshot", defaultValue = "false")
    private boolean removeSnapshot;

    /**
     * Whether to add next version number and <code>-SNAPSHOT</code> to the existing version.
     * Unless specified by <code>nextSnapshotIndexToIncrement</code>, will increment
     * the last minor index of the snapshot version, e.g. the <code>z</code> in <code>x.y.z-SNAPSHOT</code>
     *
     * @since 2.10
     */
    @Parameter(property = "nextSnapshot", defaultValue = "false")
    protected boolean nextSnapshot;

    /**
     * <p>Specifies the version index to increment when using <code>nextSnapshot</code>.
     * Will increment the (1-based, counting from the left, or the most major component) index
     * of the snapshot version, e.g. for <code>-DnextSnapshotIndexToIncrement=1</code>
     * and the version being <code>1.2.3-SNAPSHOT</code>, the new version will become <code>2.2.3-SNAPSHOT.</code></p>
     * <p>Only valid with <code>nextSnapshot</code>.</p>
     *
     * @since 2.12
     */
    @Parameter(property = "nextSnapshotIndexToIncrement")
    protected Integer nextSnapshotIndexToIncrement;

    /**
     * Whether to start processing at the local aggregation root (which might be a parent module
     * of that module where Maven is executed in, and the version change may affect parent and sibling modules).
     * Setting to false makes sure only the module (and it's submodules) where Maven is executed for is affected.
     *
     * @since 2.9
     */
    @Parameter(property = "processFromLocalAggregationRoot", defaultValue = "true")
    private boolean processFromLocalAggregationRoot;

    /**
     * Whether to update the <code>project.build.outputTimestamp<code> property in the POM when setting version.
     *
     * @since 2.10
     * @deprecated please use {@link #updateBuildOutputTimestampPolicy} instead
     */
    @Parameter(property = "updateBuildOutputTimestamp", defaultValue = "true")
    private boolean updateBuildOutputTimestamp;

    /**
     * Whether to update the <code>project.build.outputTimestamp<code> property in the POM when setting version.
     * Valid values are: <code>onchange</code>, which will only change <code>outputTimestamp</code> for changed POMs,
     * <code>always</code>, <code>never</code>.
     *
     * @since 2.12
     */
    @Parameter(property = "updateBuildOutputTimestampPolicy", defaultValue = "onchange")
    private String updateBuildOutputTimestampPolicy;

    /**
     * The changes to module coordinates. Guarded by this.
     */
    private final transient List<DefaultDependencyVersionChange> sourceChanges = new ArrayList<>();

    /**
     * The (injected) instance of {@link ProjectBuilder}
     *
     * @since 2.14.0
     */
    protected final ProjectBuilder projectBuilder;

    @Inject
    public SetMojo(
            RepositorySystem repositorySystem,
            org.eclipse.aether.RepositorySystem aetherRepositorySystem,
            ProjectBuilder projectBuilder,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders,
            Prompter prompter) {
        super(repositorySystem, aetherRepositorySystem, wagonMap, changeRecorders);
        this.projectBuilder = projectBuilder;
        this.prompter = prompter;
    }

    private synchronized void addChange(String groupId, String artifactId, String oldVersion, String newVersion) {
        if (!newVersion.equals(oldVersion)) {
            sourceChanges.add(new DefaultDependencyVersionChange(groupId, artifactId, oldVersion, newVersion));
        }
    }

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong.
     */
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (getProject().getOriginalModel().getVersion() == null) {
            throw new MojoExecutionException("Project version is inherited from parent.");
        }

        if (removeSnapshot && !nextSnapshot) {
            String version = getVersion();
            if (version.endsWith(SNAPSHOT)) {
                newVersion = version.substring(0, version.indexOf(SNAPSHOT));
                getLog().info("SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion);
            }
        }

        if (!nextSnapshot && nextSnapshotIndexToIncrement != null) {
            throw new MojoExecutionException("nextSnapshotIndexToIncrement is not valid when nextSnapshot is false");
        }

        if (!removeSnapshot && nextSnapshot) {
            String version = getVersion();
            newVersion = getIncrementedVersion(version, nextSnapshotIndexToIncrement);
            getLog().info("SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion);
        }

        if (isEmpty(newVersion)) {
            if (removeSnapshot) {
                getLog().info("removeSnapshot enabled whilst the version is not a snapshot: nothing to do.");
                return;
            }
            if (session.getSettings().isInteractiveMode()) {
                try {
                    newVersion = prompter.prompt(
                            "Enter the new version to set",
                            getProject().getOriginalModel().getVersion());
                } catch (PrompterException e) {
                    throw new MojoExecutionException(e.getMessage(), e);
                }
            } else {
                throw new MojoExecutionException("You must specify the new version, either by using the newVersion "
                        + "property (that is -DnewVersion=... on the command line) "
                        + "or run in interactive mode");
            }
        }

        if (!"onchange".equals(updateBuildOutputTimestampPolicy)
                && !"always".equals(updateBuildOutputTimestampPolicy)
                && !"never".equals(updateBuildOutputTimestampPolicy)) {
            throw new MojoExecutionException(
                    "updateBuildOutputTimestampPolicy should be one of: " + "\"onchange\", \"always\", \"never\".");
        }

        try {
            final MavenProject project = processFromLocalAggregationRoot
                    ? PomHelper.getLocalRoot(projectBuilder, session, getLog())
                    : getProject();

            getLog().info("Local aggregation root: " + project.getBasedir());
            Map<File, Model> reactorModels = PomHelper.getChildModels(project, getLog());
            final SortedMap<File, Model> reactor = new TreeMap<>(new ReactorDepthComparator(reactorModels));
            reactor.putAll(reactorModels);
            final Map<Pair<String, String>, Set<Model>> children = computeChildren(reactorModels);

            // set of files to update
            final Set<File> files = new LinkedHashSet<>();

            // groupId, artifactId, oldVersion are matched against every module of the project to see if the module
            // needs to be changed as well
            // setting them to the main project coordinates in case they are not set by the user,
            // so that the main project can be selected
            Model rootModel = reactorModels.get(session.getCurrentProject().getFile());
            if (groupId == null) {
                groupId = rootModel.getGroupId();
            }
            if (artifactId == null) {
                artifactId = rootModel.getArtifactId();
            }
            if (oldVersion == null) {
                oldVersion = rootModel.getVersion();
            }

            getLog().info(String.format(
                    "Processing change of %s:%s:%s -> %s", groupId, artifactId, oldVersion, newVersion));

            Pattern groupIdRegex = processAllModules || StringUtils.isBlank(groupId) || "*".equals(groupId)
                    ? null
                    : Pattern.compile(RegexUtils.convertWildcardsToRegex(groupId, true));
            Pattern artifactIdRegex = processAllModules || StringUtils.isBlank(artifactId) || "*".equals(artifactId)
                    ? null
                    : Pattern.compile(RegexUtils.convertWildcardsToRegex(artifactId, true));
            Pattern oldVersionIdRegex = processAllModules || StringUtils.isBlank(oldVersion) || "*".equals(oldVersion)
                    ? null
                    : Pattern.compile(RegexUtils.convertWildcardsToRegex(oldVersion, true));

            for (Model m : reactor.values()) {
                String mGroupId = PomHelper.getGroupId(m);
                String mArtifactId = PomHelper.getArtifactId(m);
                String mVersion = PomHelper.getVersion(m);

                if ((groupIdRegex == null || groupIdRegex.matcher(mGroupId).matches())
                        && (artifactIdRegex == null
                                || artifactIdRegex.matcher(mArtifactId).matches())
                        && (mVersion == null
                                || oldVersionIdRegex == null
                                || oldVersionIdRegex.matcher(mVersion).matches())
                        && !newVersion.equals(mVersion)) {
                    applyChange(
                            reactor,
                            children,
                            files,
                            mGroupId,
                            m.getArtifactId(),
                            StringUtils.isBlank(oldVersion) || "*".equals(oldVersion) ? "" : mVersion);
                }
            }

            if ("always".equals(updateBuildOutputTimestampPolicy)) {
                reactor.values().stream()
                        .map(m -> PomHelper.getModelEntry(reactor, PomHelper.getGroupId(m), PomHelper.getArtifactId(m)))
                        .filter(Objects::nonNull)
                        .map(Map.Entry::getValue)
                        .map(Model::getPomFile)
                        .forEach(files::add);
            }

            // now process all the updates
            for (File file : files) {
                process(file);
            }

        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    static Map<Pair<String, String>, Set<Model>> computeChildren(Map<File, Model> reactor) {
        return reactor.values().stream()
                .filter(v -> v.getParent() != null)
                .reduce(
                        new HashMap<>(),
                        (map, child) -> {
                            map.compute(
                                    new ImmutablePair<>(
                                            child.getParent().getGroupId(),
                                            child.getParent().getArtifactId()),
                                    (pair, set) -> Optional.ofNullable(set)
                                            .map(children -> {
                                                children.add(child);
                                                return children;
                                            })
                                            .orElse(new HashSet<Model>() {
                                                {
                                                    add(child);
                                                }
                                            }));
                            return map;
                        },
                        (m1, m2) -> {
                            m1.putAll(m2);
                            return m1;
                        });
    }

    /**
     * Returns the incremented version, with the nextSnapshotIndexToIncrement indicating the 1-based index,
     * from the left, or the most major version component, of the version string.
     *
     * @param version input version
     * @param nextSnapshotIndexToIncrement 1-based segment number to be incremented
     * @return version with the incremented index specified by nextSnapshotIndexToIncrement or last index
     * @throws MojoExecutionException thrown if the input parameters are invalid
     */
    protected String getIncrementedVersion(String version, Integer nextSnapshotIndexToIncrement)
            throws MojoExecutionException {
        String versionWithoutSnapshot =
                version.endsWith(SNAPSHOT) ? version.substring(0, version.indexOf(SNAPSHOT)) : version;
        List<String> numbers = new LinkedList<>(Arrays.asList(versionWithoutSnapshot.split("\\.")));

        if (nextSnapshotIndexToIncrement == null) {
            nextSnapshotIndexToIncrement = numbers.size();
        } else if (nextSnapshotIndexToIncrement < 1) {
            throw new MojoExecutionException("nextSnapshotIndexToIncrement cannot be less than 1");
        } else if (nextSnapshotIndexToIncrement > numbers.size()) {
            throw new MojoExecutionException(
                    "nextSnapshotIndexToIncrement cannot be greater than the last version index");
        }
        int snapshotVersionToIncrement = Integer.parseInt(numbers.remove(nextSnapshotIndexToIncrement - 1));
        numbers.add(nextSnapshotIndexToIncrement - 1, String.valueOf(snapshotVersionToIncrement + 1));

        return StringUtils.join(numbers.toArray(new String[0]), ".") + "-SNAPSHOT";
    }

    private void applyChange(
            SortedMap<File, Model> reactor,
            Map<Pair<String, String>, Set<Model>> children,
            Set<File> files,
            String groupId,
            String artifactId,
            String oldVersion) {

        getLog().debug("Applying change " + groupId + ":" + artifactId + ":" + oldVersion + " -> " + newVersion);
        addChange(groupId, artifactId, oldVersion, newVersion);

        Optional.ofNullable(PomHelper.getModelEntry(reactor, groupId, artifactId))
                .map(Map.Entry::getValue)
                .map(Model::getPomFile)
                .ifPresent(files::add);

        for (Map.Entry<File, Model> sourceEntry : reactor.entrySet()) {
            final File sourcePath = sourceEntry.getKey();
            final Model sourceModel = sourceEntry.getValue();

            getLog().debug(
                            sourcePath.length() == 0
                                    ? "Processing root module as parent"
                                    : "Processing " + sourcePath + " as a parent.");

            final String sourceGroupId = PomHelper.getGroupId(sourceModel);
            if (sourceGroupId == null) {
                getLog().warn("Module " + sourcePath + " is missing a groupId.");
                continue;
            }
            final String sourceArtifactId = PomHelper.getArtifactId(sourceModel);
            if (sourceArtifactId == null) {
                getLog().warn("Module " + sourcePath + " is missing an artifactId.");
                continue;
            }
            final String sourceVersion = PomHelper.getVersion(sourceModel);
            if (sourceVersion == null) {
                getLog().warn("Module " + sourcePath + " is missing a version.");
                continue;
            }

            files.add(sourceModel.getPomFile());

            getLog().debug("Looking for modules which use "
                    + ArtifactUtils.versionlessKey(sourceGroupId, sourceArtifactId) + " as their parent");
            Optional.ofNullable(children.get(new ImmutablePair<>(sourceGroupId, sourceArtifactId)))
                    .ifPresent(set -> set.forEach(model -> {
                        final boolean hasExplicitVersion = PomHelper.isExplicitVersion(model);
                        if ((updateMatchingVersions || !hasExplicitVersion)
                                && (StringUtils.equals(sourceVersion, PomHelper.getVersion(model)))) {
                            getLog().debug("    module is "
                                    + ArtifactUtils.versionlessKey(
                                            PomHelper.getGroupId(model), PomHelper.getArtifactId(model))
                                    + ":"
                                    + PomHelper.getVersion(model));
                            getLog().debug("    will become "
                                    + ArtifactUtils.versionlessKey(
                                            PomHelper.getGroupId(model), PomHelper.getArtifactId(model))
                                    + ":" + newVersion);
                            addChange(
                                    PomHelper.getGroupId(model),
                                    PomHelper.getArtifactId(model),
                                    PomHelper.getVersion(model),
                                    newVersion);
                        }
                    }));
        }
    }

    /**
     * Updates the pom file.
     *
     * @param pom The pom file to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong.
     * @throws javax.xml.stream.XMLStreamException            when things go wrong.
     */
    protected synchronized void update(ModifiedPomXMLEventReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        ContextualLog log = new DelegatingContextualLog(getLog());
        try {
            Model model = PomHelper.getRawModel(pom);
            log.setContext("Processing " + PomHelper.getGroupId(model) + ":" + PomHelper.getArtifactId(model));

            VersionChangerFactory versionChangerFactory = new VersionChangerFactory();
            versionChangerFactory.setPom(pom);
            versionChangerFactory.setLog(log);
            versionChangerFactory.setModel(model);

            VersionChanger changer = versionChangerFactory.newVersionChanger(
                    processParent, processProject, processDependencies, processPlugins);

            for (DefaultDependencyVersionChange versionChange : sourceChanges) {
                changer.apply(versionChange);
            }

            if (updateBuildOutputTimestamp && !"never".equals(updateBuildOutputTimestampPolicy)) {
                if ("always".equals(updateBuildOutputTimestampPolicy) || !sourceChanges.isEmpty()) {
                    // also update project.build.outputTimestamp
                    updateBuildOutputTimestamp(pom, model);
                }
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
        log.clearContext();
    }

    private void updateBuildOutputTimestamp(ModifiedPomXMLEventReader pom, Model model) throws XMLStreamException {
        String buildOutputTimestamp = model.getProperties().getProperty("project.build.outputTimestamp");

        if (buildOutputTimestamp == null || isEmpty(buildOutputTimestamp)) {
            // no Reproducible Builds output timestamp defined
            return;
        }

        if (StringUtils.isNumeric(buildOutputTimestamp)) {
            // int representing seconds since the epoch, like SOURCE_DATE_EPOCH
            buildOutputTimestamp = String.valueOf(System.currentTimeMillis() / 1000);
        } else if (buildOutputTimestamp.length() <= 1) {
            // value length == 1 means disable Reproducible Builds
            return;
        } else {
            // ISO-8601
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
            df.setTimeZone(TimeZone.getTimeZone("UTC"));
            buildOutputTimestamp = df.format(new Date());
        }

        PomHelper.setPropertyVersion(pom, null, "project.build.outputTimestamp", buildOutputTimestamp);
    }
}
