package org.codehaus.mojo.versions;

import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionsHelper;

final class DependencyArtifactIdentifier implements ArtifactIdentifier
{
    private final Dependency dependency;

    DependencyArtifactIdentifier( Dependency dependency )
    {
        this.dependency = dependency;
    }

    /**
     * Get the project group that produced the dependency, e.g.
     * <code>org.apache.maven</code>.
     *
     * @return String
     */
    @Override
    public final String getGroupId()
    {
        return dependency.getGroupId();
    }


    /**
     * Get the unique id for an artifact produced by the project
     * group, e.g.
     * <code>maven-artifact</code>.
     *
     * @return String
     */
    @Override
    public final String getArtifactId()
    {
        return dependency.getArtifactId();
    }

    /**
     * Get the version of the dependency, e.g. <code>3.2.1</code>.
     * In Maven 2, this can also be
     * specified as a range of versions.
     *
     * @return String
     */
    @Override
    public final String getVersion()
    {
        return dependency.getVersion();
    }

    @Override
    public Artifact getArtifact( MavenProject project, VersionsHelper versionsHelper ) throws MojoExecutionException
    {
        Artifact artifact = findArtifact( dependency, project.getDependencyArtifacts() );
        if ( artifact == null )
        {
            try
            {
                return versionsHelper.createDependencyArtifact( dependency );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
        }
        return artifact;
    }

    @Override
    public String toString()
    {
        final StringBuilder buf = new StringBuilder();
        buf.append( getGroupId() );
        buf.append( ':' );
        buf.append( getArtifactId() );
        if ( this.dependency.getType() != null && !dependency.getType().isEmpty() )
        {
            buf.append( ':' );
            buf.append( dependency.getType() );
        }
        else
        {
            buf.append( ":jar" );
        }
        if ( dependency.getClassifier() != null && !dependency.getClassifier().isEmpty() )
        {
            buf.append( ':' );
            buf.append( dependency.getClassifier() );
        }
        if ( this.getVersion() != null && !dependency.getVersion().isEmpty() )
        {
            buf.append( ':' );
            buf.append( getVersion() );
        }
        return buf.toString();
    }

    static Artifact findArtifact( Dependency dependency, Iterable<Artifact> artifacts )
    {
        if ( artifacts == null )
        {
            return null;
        }

        for ( final Artifact artifact : artifacts )
        {
            if ( compare( artifact, dependency ) )
            {
                return artifact;
            }
        }

        return null;
    }

    /**
     * Compare and artifact to a dependency. Returns true only if the groupId, artifactId, type, and classifier are all
     * equal.
     *
     * @return true if artifact and dep refer to the same artifact
     */
    private static boolean compare( Artifact artifact, Dependency dep )
    {
        return StringUtils.equals( artifact.getGroupId(), dep.getGroupId() )
                && StringUtils.equals( artifact.getArtifactId(), dep.getArtifactId() )
                && StringUtils.equals( artifact.getType(), dep.getType() )
                && StringUtils.equals( artifact.getClassifier(), dep.getClassifier() );
    }
}