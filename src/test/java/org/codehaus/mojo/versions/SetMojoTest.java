package org.codehaus.mojo.versions;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

public class SetMojoTest
{
    @Test
    public void testGetIncrementedVersion() throws MojoExecutionException
    {
        new SetMojo()
        {
            {
                assertThat( getIncrementedVersion( "1.0.0", null ), is( "1.0.1-SNAPSHOT" ) );
                assertThat( getIncrementedVersion( "1.0.0-SNAPSHOT", null ), is( "1.0.1-SNAPSHOT" ) );
                assertThat( getIncrementedVersion( "1.0.0-SNAPSHOT", 1 ), is( "2.0.0-SNAPSHOT" ) );
                assertThat( getIncrementedVersion( "1.0.0-SNAPSHOT", 2 ), is( "1.1.0-SNAPSHOT" ) );
                assertThat( getIncrementedVersion( "1.0.0-SNAPSHOT", 3 ), is( "1.0.1-SNAPSHOT" ) );
            }
        };
    }

    @Test
    public void testNextSnapshotIndexLowerBound()
    {
        new SetMojo()
        {
            {
                try
                {
                    getIncrementedVersion( "1.0.0", 0 );
                    fail();
                }
                catch ( MojoExecutionException e )
                {
                    assertThat( e.getMessage(),
                            containsString( "nextSnapshotIndexToIncrement cannot be less than 1" ) );
                }
            }
        };
    }

    @Test
    public void testNextSnapshotIndexUpperBound()
    {
        new SetMojo()
        {
            {
                try
                {
                    getIncrementedVersion( "1.0.0", 4 );
                    fail();
                }
                catch ( MojoExecutionException e )
                {
                    assertThat( e.getMessage(), containsString(
                            "nextSnapshotIndexToIncrement cannot be greater than the last version index" ) );
                }
            }
        };
    }

    @Test
    public void testNextSnapshotIndexWithoutNextSnapshot() throws MojoFailureException
    {
        try
        {
            new SetMojo()
            {
                {
                    project = new MavenProject();
                    project.setParent( new MavenProject() );
                    project.setOriginalModel( new Model() );
                    project.getOriginalModel().setVersion( "1.2.3-SNAPSHOT" );

                    nextSnapshotIndexToIncrement = 4;
                }
            }.execute();
        }
        catch ( MojoExecutionException e )
        {
            assertThat( e.getMessage(),
                    containsString( "nextSnapshotIndexToIncrement is not valid when nextSnapshot is false" ) );
        }
    }

}
