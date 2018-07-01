package org.codehaus.mojo.versions;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public abstract class ParentUpdatingDependencyUpdateMojo extends AbstractVersionsDependencyUpdaterMojo
{
    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
       throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                setVersions( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                setVersions( pom, getProject().getDependencies() );
            }
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                final Dependency dependency = new Dependency();
                dependency.setArtifactId(getProject().getParent().getArtifactId());
                dependency.setGroupId(getProject().getParent().getGroupId());
                dependency.setVersion(getProject().getParent().getVersion());
                dependency.setType("pom");
                setVersions( pom, Collections.singleton(dependency));
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    protected abstract void setVersions(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException;

    protected void setVersion(ModifiedPomXMLEventReader pom, Dependency dep, String version, Artifact artifact, ArtifactVersion artifactVersion) throws XMLStreamException
    {
        final String newVersion = artifactVersion.toString();
        if(getProject().getParent() != null){
            if(artifact.getId().equals(getProject().getParentArtifact().getId()) && isProcessingParent())
            {
                if ( PomHelper.setProjectParentVersion( pom, newVersion.toString() ) )
                {
                    getLog().debug( "Made parent change from " + version + " to " + newVersion.toString() );
                }
            }
        }

        if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                             newVersion, getProject().getModel() ) )
        {
            getLog().info( "Changed " + toString( dep ) + " to version " + newVersion );
        }
    }
}
