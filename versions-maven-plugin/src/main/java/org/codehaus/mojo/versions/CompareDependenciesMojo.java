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
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingResult;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;
import static java.util.Optional.ofNullable;

/**
 * Compare dependency versions of the current project to dependencies or dependency management of a remote repository
 * project. Can optionally update locally the project instead of reporting the comparison
 *
 * @author Paul Gier
 * @since 1.3
 */
@Mojo(name = "compare-dependencies", threadSafe = true)
public class CompareDependenciesMojo extends AbstractVersionsDependencyUpdaterMojo {

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 68;

    /**
     * The groupId, artifactId, and version of the remote project (POM) to which we are comparing. This should be in the
     * form "groupId:artifactId:version"
     */
    @Parameter(property = "remotePom", required = true)
    protected String remotePom;

    /**
     * Ignore the list of remote dependencies and only compare the remote dependencyManagement
     */
    @Parameter(property = "ignoreRemoteDependencies", defaultValue = "false")
    protected boolean ignoreRemoteDependencies;

    /**
     * Ignore the remote dependency management and only check against the actual dependencies of the remote project
     */
    @Parameter(property = "ignoreRemoteDependencyManagement", defaultValue = "false")
    protected boolean ignoreRemoteDependencyManagement;

    /**
     * Update dependency versions in the current POM.
     */
    @Parameter(property = "updateDependencies", defaultValue = "false")
    protected boolean updateDependencies;

    /**
     * Update dependency versions stored in properties
     */
    @Parameter(property = "updatePropertyVersions", defaultValue = "false")
    protected boolean updatePropertyVersions;

    /**
     * Display the dependency version differences on the command line, but do not update the versions in the current
     * pom. If updateDependencies is set to "true" this will automatically be set to false.
     */
    @Parameter(property = "reportMode", defaultValue = "true")
    protected boolean reportMode;

    /**
     * If the output file is set, the diff report will be written to this file.
     */
    @Parameter(property = "reportOutputFile")
    protected File reportOutputFile;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    private boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    private boolean processDependencyManagement = true;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter(property = "processParent", defaultValue = "false")
    private boolean processParent = false;

    /**
     * The (injected) instance of {@link ProjectBuilder}
     *
     * @since 2.14.0
     */
    protected final ProjectBuilder projectBuilder;

    // ------------------------------ METHODS --------------------------

    @Inject
    public CompareDependenciesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ProjectBuilder projectBuilder,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
        this.projectBuilder = projectBuilder;
    }

    @Override
    protected boolean getProcessDependencies() {
        return processDependencies;
    }

    @Override
    protected boolean getProcessDependencyManagement() {
        return processDependencyManagement;
    }

    @Override
    public boolean getProcessParent() {
        return processParent;
    }

    @Override
    protected boolean getAllowSnapshots() {
        // this parameter is used by base class, but shouldn't affect comparison; setting to true
        return true;
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException Something wrong with the plugin itself
     * @throws org.apache.maven.plugin.MojoFailureException   The plugin detected an error in the build
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        if (this.ignoreRemoteDependencies && this.ignoreRemoteDependencyManagement) {
            throw new MojoFailureException(" ignoreRemoteDependencies and ignoreRemoteDependencyManagement "
                    + "are both set to true.  At least one of these needs to be false ");
        }

        if (updateDependencies) {
            reportMode = false;
        }

        String[] remoteGAV = this.remotePom.split(":");
        if (remoteGAV.length != 3) {
            throw new MojoFailureException(" Invalid format for remotePom: " + remotePom);
        }

        MavenProject remoteMavenProject = null;
        try {
            remoteMavenProject = getRemoteMavenProject(remoteGAV[0], remoteGAV[1], remoteGAV[2]);
        } catch (ArtifactResolutionException | ProjectBuildingException e) {
            throw new MojoFailureException(e.getMessage());
        }

        Map<String, Dependency> remoteDepsMap = new HashMap<>();
        if (!ignoreRemoteDependencyManagement) {
            List<Dependency> remoteProjectDepMgmtDeps = remoteMavenProject.getDependencyManagement() == null
                    ? null
                    : remoteMavenProject.getDependencyManagement().getDependencies();
            if (remoteProjectDepMgmtDeps != null) {
                remoteProjectDepMgmtDeps.forEach(dep -> remoteDepsMap.putIfAbsent(dep.getManagementKey(), dep));
            }
        }
        if (!ignoreRemoteDependencies && remoteMavenProject.getDependencies() != null) {
            remoteMavenProject.getDependencies().forEach(dep -> remoteDepsMap.putIfAbsent(dep.getManagementKey(), dep));
        }

        List<String> totalDiffs = new ArrayList<>();
        List<String> propertyDiffs = new ArrayList<>();
        if (getProject().getDependencyManagement() != null && getProcessDependencyManagement()) {
            totalDiffs.addAll(compareVersions(
                    pom,
                    getProject().getDependencyManagement().getDependencies(),
                    remoteDepsMap,
                    DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT));
        }
        if (getProject().getDependencies() != null && getProcessDependencies()) {
            totalDiffs.addAll(compareVersions(
                    pom, getProject().getDependencies(), remoteDepsMap, DependencyChangeRecord.ChangeKind.DEPENDENCY));
        }
        if (updatePropertyVersions) {
            Map<Property, PropertyVersions> versionProperties = this.getHelper()
                    .getVersionPropertiesMap(VersionsHelper.VersionPropertiesMapRequest.builder()
                            .withMavenProject(getProject())
                            .build());
            propertyDiffs.addAll(updatePropertyVersions(pom, versionProperties, remoteDepsMap));
        }
        if (getProject().getParent() != null && remoteMavenProject.getParent() != null && getProcessParent()) {
            Dependency parent = DependencyBuilder.newBuilder()
                    .withGroupId(remoteMavenProject.getParentArtifact().getGroupId())
                    .withArtifactId(remoteMavenProject.getParentArtifact().getArtifactId())
                    .withVersion(remoteMavenProject.getParentArtifact().getVersion())
                    .withType(remoteMavenProject.getParentArtifact().getType())
                    .withScope(remoteMavenProject.getParentArtifact().getScope())
                    .withClassifier(remoteMavenProject.getParentArtifact().getClassifier())
                    .build();
            if (getLog().isDebugEnabled()) {
                getLog().debug("Processing parent dependency: " + parent);
            }
            remoteDepsMap.putIfAbsent(parent.getManagementKey(), parent);
            totalDiffs.addAll(compareVersions(
                    pom,
                    singletonList(getParentDependency()),
                    remoteDepsMap,
                    DependencyChangeRecord.ChangeKind.PARENT));
        }

        if (reportMode) {
            getLog().info("The following differences were found:");
            if (totalDiffs.isEmpty()) {
                getLog().info("  none");
            } else {
                for (String totalDiff : totalDiffs) {
                    getLog().info("  " + totalDiff);
                }
            }
            getLog().info("The following property differences were found:");
            if (propertyDiffs.isEmpty()) {
                getLog().info("  none");
            } else {
                for (String propertyDiff : propertyDiffs) {
                    getLog().info("  " + propertyDiff);
                }
            }
        }

        if (reportOutputFile != null) {
            writeReportFile(totalDiffs, propertyDiffs);
        }
    }

    /**
     * Builds a {@link MavenProject} instance for the dependency with a given {@code groupId},
     * {@code artifactId}, and {@code version}.
     * @param groupId {@code groupId} of the dependency
     * @param artifactId {@code artifactId} of the dependency
     * @param version {@code version} of the dependency
     * @return retrieved {@link MavenProject} instance for the given dependency
     * @throws MojoExecutionException thrown if the artifact for the dependency could not be constructed
     * @throws ProjectBuildingException thrown if the {@link MavenProject} instance could not be constructed
     */
    private MavenProject getRemoteMavenProject(String groupId, String artifactId, String version)
            throws MojoExecutionException, ArtifactResolutionException, ProjectBuildingException {
        Artifact remoteArtifact = toArtifact(DependencyBuilder.newBuilder()
                .withGroupId(groupId)
                .withArtifactId(artifactId)
                .withVersion(version)
                .build());
        ProjectBuildingResult result = projectBuilder.build(
                remoteArtifact,
                true,
                PomHelper.createProjectBuilderRequest(
                        session,
                        r -> r.setProcessPlugins(false),
                        r -> r.setRemoteRepositories(session.getCurrentProject().getRemoteArtifactRepositories()),
                        r -> r.setPluginArtifactRepositories(
                                session.getCurrentProject().getPluginArtifactRepositories())));
        if (!result.getProblems().isEmpty()) {
            getLog().warn("Problems encountered during construction of the POM for " + remoteArtifact.toString());
            result.getProblems().forEach(p -> getLog().warn("\t" + p.getMessage()));
        }
        return result.getProject();
    }

    /**
     * Compare the dependency versions of the current project with the dependency versions of a remote project
     *
     * @throws XMLStreamException
     * @throws MojoExecutionException
     */
    private List<String> compareVersions(
            MutableXMLStreamReader pom,
            List<Dependency> dependencies,
            Map<String, Dependency> remoteDependencies,
            DependencyChangeRecord.ChangeKind changeKind)
            throws MojoExecutionException, XMLStreamException {
        List<String> updates = new ArrayList<>();
        for (Dependency dep : dependencies) {
            Artifact artifact = this.toArtifact(dep);
            if (!isIncluded(artifact)) {
                continue;
            }

            Dependency remoteDep = remoteDependencies.get(dep.getManagementKey());
            if (remoteDep != null) {
                String remoteVersion = remoteDep.getVersion();
                if (!Objects.equals(remoteVersion, dep.getVersion())) {
                    StringBuilder buf = writeDependencyDiffMessage(dep, remoteVersion);
                    updates.add(buf.toString());
                    if (!reportMode) {
                        updateDependencyVersion(pom, dep, remoteVersion, changeKind);
                    }
                }
            }
        }

        return updates;
    }

    /**
     * Updates the properties holding a version if necessary.
     */
    private List<String> updatePropertyVersions(
            MutableXMLStreamReader pom,
            Map<Property, PropertyVersions> versionProperties,
            Map<String, Dependency> remoteDependencies)
            throws XMLStreamException {
        List<String> result = new ArrayList<>();
        for (Map.Entry<Property, PropertyVersions> entry : versionProperties.entrySet()) {
            Property property = entry.getKey();
            PropertyVersions version = entry.getValue();

            String candidateVersion = computeCandidateVersion(remoteDependencies, property, version);
            if (candidateVersion != null) {
                String originalVersion =
                        version.getAssociations()[0].getArtifact().getVersion(); // Yekes
                if (!candidateVersion.equals(originalVersion)) // Update needed
                {
                    result.add(writeDiffMessage(property.getName(), originalVersion, candidateVersion)
                            .toString());
                    if (!reportMode && PomHelper.setPropertyVersion(pom, null, property.getName(), candidateVersion)) {
                        getLog().info("Updated ${" + property.getName() + "} from " + originalVersion + " to "
                                + candidateVersion);
                    }
                }
            }
        }
        return result;
    }

    /**
     * Returns the candidate version to use for the specified property.
     * <p/>
     * The dependencies currently linked to the property must all be defined by the remote POM and they should refer to
     * the same version. If that's the case, that same version is returned. Otherwise, <tt>null</tt> is returned
     * indicating that there is no candidate.
     *
     * @param remoteDependencies the remote dependencies
     * @param property           the property to update
     * @param propertyVersions   the association
     * @return the candidate version or <tt>null</tt> if there isn't any
     */
    private String computeCandidateVersion(
            Map<String, Dependency> remoteDependencies, Property property, PropertyVersions propertyVersions) {
        String candidateVersion = null;
        for (ArtifactAssociation artifactAssociation : propertyVersions.getAssociations()) {
            String id = generateId(artifactAssociation.getArtifact());
            Dependency dependency = remoteDependencies.get(id);
            if (dependency == null) {
                getLog().info("Not updating ${" + property.getName() + "}: no info for " + id);
                return null;
            } else {
                if (candidateVersion == null) {
                    candidateVersion = dependency.getVersion();
                } else if (!candidateVersion.equals(dependency.getVersion())) {
                    getLog().warn("Could not update ${" + property.getName() + "}: version mismatch");
                    return null;
                }
            }
        }
        return candidateVersion;
    }

    private void writeReportFile(List<String> dependenciesUpdate, List<String> propertiesUpdate)
            throws MojoExecutionException {
        if (!reportOutputFile.getParentFile().exists()) {
            reportOutputFile.getParentFile().mkdirs();
        }

        try (FileWriter fw = new FileWriter(reportOutputFile); //
                PrintWriter pw = new PrintWriter(fw)) {
            pw.println("The following differences were found:");
            pw.println();
            if (dependenciesUpdate.isEmpty()) {
                pw.println("  none");
            } else {
                for (String dependencyUpdate : dependenciesUpdate) {
                    pw.println("  " + dependencyUpdate);
                }
            }
            pw.println();
            pw.println("The following property differences were found:");
            pw.println();
            if (propertiesUpdate.isEmpty()) {
                pw.println("  none");
            } else {
                for (String propertyUpdate : propertiesUpdate) {
                    pw.println("  " + propertyUpdate);
                }
            }
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to write report file. ", e);
        }
    }

    /**
     * Create a simple message describing the version diff
     *
     * @param dep
     * @param remoteVersion
     * @return The message
     */
    private StringBuilder writeDependencyDiffMessage(Dependency dep, String remoteVersion) {
        String id = dep.getGroupId() + ":" + dep.getArtifactId();
        return writeDiffMessage(
                id,
                ofNullable(dep.getVersion()).orElse("(no version)"),
                ofNullable(remoteVersion).orElse("(no version)"));
    }

    private StringBuilder writeDiffMessage(String id, String originalVersion, String targetVersion) {
        StringBuilder buf = new StringBuilder();
        buf.append(id);
        buf.append(' ');
        int padding = INFO_PAD_SIZE - originalVersion.length() - targetVersion.length() - 4;
        while (buf.length() < padding) {
            buf.append('.');
        }
        buf.append(' ');
        buf.append(originalVersion);
        buf.append(" -> ");
        buf.append(targetVersion);
        return buf;
    }

    /**
     * Creates a key that is similar to what {@link Dependency#getManagementKey()} generates for a dependency.
     */
    private static String generateId(Artifact artifact) {
        StringBuilder sb = new StringBuilder();
        sb.append(artifact.getGroupId())
                .append(":")
                .append(artifact.getArtifactId())
                .append(":")
                .append(artifact.getType());
        if (artifact.getClassifier() != null) {
            sb.append(":").append(artifact.getClassifier());
        }
        return sb.toString();
    }
}
