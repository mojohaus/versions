package org.codehaus.mojo.versions;

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.model.Scm;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import static org.apache.commons.lang3.StringUtils.isAllBlank;
import static org.apache.commons.lang3.StringUtils.isBlank;

/**
 * Updates the current project's SCM tag.
 *
 * @author Anton Johansson
 * @since 2.5
 */
@Mojo( name = "set-scm-tag", requiresDirectInvocation = true, aggregator = true, threadSafe = true )
public class SetScmTagMojo extends AbstractVersionsUpdaterMojo
{

    /**
     * The new SCM tag to set.
     *
     * @since 2.5
     */
    @Parameter( property = "newTag" )
    private String newTag;

    /**
     * The new SCM connection property
     *
     * @since 2.12.0
     */
    @Parameter( property = "connection" )
    private String connection;

    /**
     * The new SCM developerConnection property
     *
     * @since 2.12.0
     */
    @Parameter( property = "developerConnection" )
    private String developerConnection;

    /**
     * The new SCM url property
     *
     * @since 2.12.0
     */
    @Parameter( property = "url" )
    private String url;

    @Inject
    public SetScmTagMojo( RepositorySystem repositorySystem,
                                           MavenProjectBuilder projectBuilder,
                                           ArtifactMetadataSource artifactMetadataSource,
                                           WagonManager wagonManager,
                                           ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong.
     */
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException
    {
        if ( isAllBlank( newTag, connection, developerConnection, url ) )
        {
            throw new MojoFailureException(
                    "One of: \"newTag\", \"connection\", \"developerConnection\", \"url\" should be provided." );
        }

        super.execute();
    }

    @Override
    protected void update( ModifiedPomXMLEventReader pom )
            throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {
        try
        {
            Scm scm = PomHelper.getRawModel( pom ).getScm();
            if ( scm == null )
            {
                throw new MojoFailureException( "No <scm> was present" );
            }

            List<String> failures = new ArrayList<>();
            if ( !isBlank( newTag ) )
            {
                getLog().info( "Updating tag: " + ( scm != null && scm.getTag() != null
                        ? scm.getTag() : "(empty)" ) + " -> " + newTag );
                if ( !PomHelper.setElementValue( pom, "/project/scm", "tag", newTag ) )
                {
                    failures.add( "tag: " + newTag );
                }
            }
            if ( !isBlank( connection ) )
            {
                getLog().info( "Updating connection: " + ( scm != null && scm.getConnection() != null
                        ? scm.getConnection() : "(empty)" ) + " -> " + connection );
                if ( !PomHelper.setElementValue( pom, "/project/scm", "connection", connection ) )
                {
                    failures.add( "connection: " + connection );
                }
            }
            if ( !isBlank( developerConnection ) )
            {
                getLog().info( "Updating developerConnection: "
                        + ( scm != null && scm.getDeveloperConnection() != null
                        ? scm.getDeveloperConnection() : "(empty)" ) + " -> "
                        + developerConnection );
                if ( !PomHelper.setElementValue( pom, "/project/scm", "developerConnection",
                        developerConnection ) )
                {
                    failures.add( "developerConnection: " + developerConnection );
                }
            }
            if ( !isBlank( url ) )
            {
                getLog().info( "Updating url: " + ( scm != null && scm.getUrl() != null
                        ? scm.getUrl() : "(empty)" ) + " -> " + url );
                if ( !PomHelper.setElementValue( pom, "/project/scm", "url", url ) )
                {
                    failures.add( "url: " + url );
                }
            }
            if ( !failures.isEmpty() )
            {
                throw new MojoFailureException( "Could not update one or more SCM elements: " + String.join( ", ",
                        failures ) + ". Please make sure they are present in the original POM. " );
            }
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }
}
