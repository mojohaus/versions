package org.codehaus.mojo.versions.utils;

import javax.inject.Inject;
import javax.inject.Named;

import java.io.File;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;

/**
 * Factory for creating {@link Artifact} instances.
 */
@Named
public class ArtifactFactory {
    private final ArtifactHandlerManager artifactHandlerManager;

    /**
     * Constructs a new instance
     * @param artifactHandlerManager {@link ArtifactHandlerManager} instance
     */
    @Inject
    public ArtifactFactory(ArtifactHandlerManager artifactHandlerManager) {
        this.artifactHandlerManager = artifactHandlerManager;
    }

    /**
     * Creates a new {@link Artifact} instance with the given parameters.
     * The version parameter may be a version range.
     * @param groupId the groupId
     * @param artifactId the artifactId
     * @param version the version or version range
     * @param type the type
     * @param classifier the classifier
     * @param scope the scope
     * @param optional whether the dependency is optional
     * @return the created {@link Artifact} instance
     * @throws RuntimeException if the version parameter is not a valid version or version range
     * (in this case the cause will be an {@link InvalidVersionSpecificationException})
     */
    public Artifact createArtifact(
            String groupId,
            String artifactId,
            String version,
            String type,
            String classifier,
            String scope,
            boolean optional) {
        try {
            return new DefaultArtifact(
                    groupId,
                    artifactId,
                    VersionRange.createFromVersionSpec(StringUtils.isNotBlank(version) ? version : "[0,]"),
                    scope,
                    type,
                    classifier,
                    artifactHandlerManager.getArtifactHandler(type),
                    optional);
        } catch (InvalidVersionSpecificationException e) {
            // version should have a proper format
            throw new RuntimeException(e);
        }
    }

    /**
     * Creates a new "maven-plugin"-type artifact with the given coordinates. The scope will be {@code runtime}.
     * @param groupId the groupId
     * @param artifactId the artifactId
     * @param version the version
     * @return the created {@link Artifact} instance
     */
    public Artifact createMavenPluginArtifact(String groupId, String artifactId, String version) {
        return createArtifact(groupId, artifactId, version, "maven-plugin", null, "runtime", false);
    }

    /**
     * Creates an {@link Artifact} object based on a {@link Dependency} instance
     * @param dependency the dependency
     * @return the created {@link Artifact} instance
     */
    public Artifact createArtifact(Dependency dependency) {
        Artifact artifact = createArtifact(
                dependency.getGroupId(),
                dependency.getArtifactId(),
                dependency.getVersion(),
                dependency.getType(),
                dependency.getClassifier(),
                dependency.getScope(),
                false);

        if (Artifact.SCOPE_SYSTEM.equals(dependency.getScope()) && dependency.getSystemPath() != null) {
            artifact.setFile(new File(dependency.getSystemPath()));
        }
        return artifact;
    }
}
