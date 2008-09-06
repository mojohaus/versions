package org.codehaus.mojo.versions;

import org.apache.maven.model.Profile;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Scans the current projects child modules, updating the versions of any which use the current project to
 * the version of the current project.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-child-modules
 * @aggregator
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0
 */
public class UpdateChildModulesMojo
    extends AbstractMojo
{

    /**
     * @parameter expression="${reactorProjects}"
     * @required
     * @readonly
     * @since 1.0
     */
    protected List reactorProjects;

    /**
     * @parameter expression="${project}"
     * @required
     * @readonly
     * @since 1.0
     */
    private MavenProject project;

    public void execute()
        throws MojoExecutionException, MojoFailureException
    {

        Set childModules = getAllChildModules( project );

        removeMissingChildModules( project, childModules );

        throw new UnsupportedOperationException( "Implement the remainder of this." );
    }

    private Set getAllChildModules( MavenProject project )
    {
        getLog().debug( "Finding child modules..." );
        Set childModules = new TreeSet();
        childModules.addAll( project.getOriginalModel().getModules() );
        Iterator i = project.getOriginalModel().getProfiles().iterator();
        while ( i.hasNext() )
        {
            Profile profile = (Profile) i.next();
            childModules.addAll( profile.getModules() );
        }
        debugModules( "Child modules:", childModules );
        return childModules;
    }

    private void debugModules( String message, Set childModules )
    {
        Iterator i;
        if ( getLog().isDebugEnabled() )
        {
            getLog().debug( message );
            if ( childModules.isEmpty() )
            {
                getLog().debug( "None." );
            }
            else
            {
                i = childModules.iterator();
                while ( i.hasNext() )
                {
                    getLog().debug( "  " + i.next() );
                }
            }

        }
    }

    private void removeMissingChildModules( MavenProject project, Set childModules )
    {
        getLog().debug( "Removing child modules which are missing..." );
        Iterator i = childModules.iterator();
        while ( i.hasNext() )
        {
            String modulePath = (String) i.next();
            File moduleFile = new File( project.getBasedir(), modulePath );
            if ( !moduleFile.exists() )
            {
                getLog().debug( "Removing missing child module " + modulePath );
                i.remove();
            }
        }
        debugModules( "After removing missing", childModules );
    }
}
