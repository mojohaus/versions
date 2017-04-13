package org.codehaus.mojo.versions;

import java.util.Collections;
import java.util.LinkedList;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.codehaus.plexus.util.StringUtils;

/**
 * Sets the current project's version and based on that change propagates that change onto any child modules as
 * necessary.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo (name="set", requiresProject = true, requiresDirectInvocation = true, aggregator = true)
public class SetMojo
    extends AbstractModuleVersionUpdaterMojo
{

    /**
     * The new version number to set.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "newVersion") String newVersion;

    /**
     * Component used to prompt for input
     *
     * @component
     */
    @Component
    private Prompter prompter;

    /**
     * Whether to remove -SNAPSHOT from the existing version. If not set will default to false.
     *
     * @since 2.10
     */
    @Parameter(property = "removeSnapshot", defaultValue = "false")
    private boolean removeSnapshot;

    /**
     * Whether to add next version number and -SNAPSHOT to the existing version. If not set will default to false.
     *
     * @since 2.10
     */
    @Parameter(property = "nextSnapshot", defaultValue = "false")
    private boolean nextSnapshot;

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong.
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {

        if ( getProject().getOriginalModel().getVersion() == null )
        {
            throw new MojoExecutionException( "Project version is inherited from parent." );
        }

        if ( removeSnapshot == true && nextSnapshot == false ) {
            String version = getVersion();
            String release = version;
            if (version.endsWith( SNAPSHOT)) {
                release = version.substring( 0, version.indexOf( SNAPSHOT ) );
                newVersion = release;
                getLog().info("SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion);
            }
        }

        if ( removeSnapshot == false && nextSnapshot == true )
        {
            boolean havingSnapshot = false;
            String version = getVersion();
            String versionWithoutSnapshot = version;
            if ( version.endsWith( SNAPSHOT ) )
            {
                havingSnapshot = true;
                versionWithoutSnapshot = version.substring( 0, version.indexOf( SNAPSHOT ) );
            }
            LinkedList<String> numbers = new LinkedList<String>();
            if ( versionWithoutSnapshot.contains( "." ) )
            {
                // Chop the version into numbers by splitting on the dot (.)
                Collections.addAll( numbers, versionWithoutSnapshot.split( "\\." ) );
            }
            else
            {
                // The version contains no dots, assume that it is only 1 number
                numbers.add( versionWithoutSnapshot );
            }

            int lastNumber = Integer.parseInt( numbers.removeLast() );
            numbers.addLast( String.valueOf( lastNumber + 1 ) );
            String nextVersion = StringUtils.join( numbers.toArray( new String[0] ), "." );
            if ( havingSnapshot )
            {
                newVersion = nextVersion + "-SNAPSHOT";
            }
            getLog().info( "SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion );
        }

        if ( StringUtils.isEmpty( newVersion ) )
        {
            if ( settings.isInteractiveMode() )
            {
                try
                {
                    newVersion =
                        prompter.prompt( "Enter the new version to set", getProject().getOriginalModel().getVersion() );
                }
                catch ( PrompterException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
            }
            else
            {
                throw new MojoExecutionException( "You must specify the new version, either by using the newVersion "
                    + "property (that is -DnewVersion=... on the command line) or run in interactive mode" );
            }
        }
        if ( StringUtils.isEmpty( newVersion ) )
        {
            throw new MojoExecutionException( "You must specify the new version, either by using the newVersion "
                + "property (that is -DnewVersion=... on the command line) or run in interactive mode" );
        }

        setVersion(newVersion);
    }

}
