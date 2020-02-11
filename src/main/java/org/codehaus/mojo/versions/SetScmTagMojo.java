package org.codehaus.mojo.versions;

import static org.apache.commons.lang3.StringUtils.isBlank;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.model.Model;
import org.apache.maven.model.Scm;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Updates the current project's SCM tag.
 *
 * @author Anton Johansson
 * @since 2.5
 */
@Mojo( name = "set-scm-tag", requiresDirectInvocation = true, aggregator = true, threadSafe = true )
public class SetScmTagMojo
    extends AbstractVersionsUpdaterMojo
{

    /**
     * The new SCM tag to set.
     *
     * @since 2.5
     */
    @Parameter( property = "newTag" )
    private String newTag;

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong.
     */
	@Override
    public void execute()
	    throws MojoExecutionException, MojoFailureException
	{
	    if ( isBlank(newTag) )
	    {
	        throw new MojoFailureException("'newTag' cannot be empty");
	    }

	    super.execute();
	}

    @Override
    protected void update(ModifiedPomXMLEventReader pom) throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {
        try
        {
            Model model = PomHelper.getRawModel( pom );
            Scm scm = model.getScm();
            if (scm == null)
            {
                throw new MojoFailureException( "No <scm> was present" );
            }
            getLog().info( "Updating from tag " + scm.getTag() + " > " + newTag );

            boolean success = PomHelper.setProjectValue(pom, "/project/scm/tag", newTag );
            if ( !success )
            {
                throw new MojoFailureException( "Could not update the SCM tag" );
            }
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }
}
