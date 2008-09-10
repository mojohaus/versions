package org.codehaus.mojo.versions;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Parent;
import org.apache.maven.model.Profile;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.profiles.DefaultProfileManager;
import org.apache.maven.profiles.ProfileManager;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuildingException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;
import java.io.File;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;
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
    extends AbstractVersionsUpdaterMojo
{

    /**
     * @parameter expression="${session}"
     * @readonly
     * @since 1.0
     */
    private MavenSession session;

    public void execute()
        throws MojoExecutionException, MojoFailureException
    {

        ProfileManager profileManager =
            new DefaultProfileManager( session.getContainer(), session.getExecutionProperties() );

        Set childModules = getAllChildModules( getProject() );

        removeMissingChildModules( getProject(), childModules );

        Iterator i = childModules.iterator();

        MojoExecutionException pbe = null;

        while ( i.hasNext() )
        {
            String modulePath = (String) i.next();

            File moduleDir = new File( getProject().getBasedir(), modulePath );

            File moduleProjectFile;

            if ( moduleDir.isDirectory() )
            {
                moduleProjectFile = new File( moduleDir, "pom.xml" );
            }
            else
            {
                // i don't think this should ever happen... but just in case
                // the module references the file-name
                moduleProjectFile = moduleDir;
            }

            try
            {
                MavenProject childProject = projectBuilder.build( moduleProjectFile, localRepository, profileManager );
                Parent childParent = childProject.getOriginalModel().getParent();
                if ( childParent != null && getProject().getGroupId().equals( childParent.getGroupId() ) &&
                    getProject().getArtifactId().equals( childParent.getArtifactId() ) )
                {
                    if ( childProject.getParent().getVersion().equals( getProject().getVersion() ) )
                    {
                        getLog().info( modulePath + " => " +
                            ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":" +
                            getProject().getVersion() );
                    }
                    else
                    {
                        getLog().warn( modulePath + " => " +
                            ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":" +
                            childProject.getParent().getVersion() + " -> " + getProject().getVersion() );
                        process( moduleProjectFile );
                    }
                }
            }
            catch ( ProjectBuildingException e )
            {
                getLog().debug( "Could not parse " + moduleProjectFile.getPath(), e );
                if ( pbe == null )
                {
                    // save this until we get to the end.
                    pbe = new MojoExecutionException( "Could not parse " + moduleProjectFile.getPath(), e );
                }
            }
        }

        if ( pbe != null )
        {
            // ok, now throw the first one to blow up.
            throw pbe;
        }

    }

    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        getLog().debug( "Updating parent to " + getProject().getVersion() );

        Stack stack = new Stack();
        String path = "";

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path = new StringBuffer()
                    .append( path )
                    .append( "/" )
                    .append( event.asStartElement().getName().getLocalPart() )
                    .toString();

                if ( "/project/parent/version".equals( path ) )
                {
                    pom.mark( 0 );
                }
            }
            if ( event.isEndElement() )
            {
                if ( "/project/parent/version".equals( path ) )
                {
                    pom.mark( 1 );
                    if ( pom.hasMark( 0 ) )
                    {
                        pom.replaceBetween( 0, 1, getProject().getVersion() );
                        getLog().debug( "Made an update to " + getProject().getVersion() );
                        return;
                    }
                }
                path = (String) stack.pop();
            }
        }
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
