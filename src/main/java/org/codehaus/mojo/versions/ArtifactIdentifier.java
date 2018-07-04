package org.codehaus.mojo.versions;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionsHelper;

/**
 * Abstraction identifying an artifact
 *
 * This allows the same code to handle either plugins or dependencies
 */
interface ArtifactIdentifier
{
    String getGroupId();

    String getArtifactId();

    String getVersion();

    /**
     * Gets the Artifact that represented by this instance.
     */
    Artifact getArtifact( MavenProject project, VersionsHelper versionsHelper ) throws MojoExecutionException;

    @Override
    String toString();
}
