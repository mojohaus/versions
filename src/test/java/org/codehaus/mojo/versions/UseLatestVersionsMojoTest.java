package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import java.util.Arrays;
import java.util.Collections;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.change.VersionChange;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

@SuppressWarnings( "deprecation" )
public class UseLatestVersionsMojoTest extends AbstractMojoTestCase
{
    private UseLatestVersionsMojo mojo;
    private TestChangeRecorder changeRecorder;

    @Before
    public void setUp() throws Exception
    {
        super.setUp();
        RepositorySystem repositorySystemMock = mock( RepositorySystem.class );
        when( repositorySystemMock.createDependencyArtifact( any( Dependency.class ) ) ).thenAnswer( invocation ->
        {
            Dependency dependency = invocation.getArgument( 0 );
            return new DefaultArtifact( dependency.getGroupId(), dependency.getArtifactId(), dependency.getVersion(),
                    dependency.getScope(), dependency.getType(),
                    dependency.getClassifier() != null ? dependency.getClassifier() : "default", null );
        } );

        ArtifactMetadataSource artifactMetadataSourceMock = mock( ArtifactMetadataSource.class );
        when( artifactMetadataSourceMock.retrieveAvailableVersions( any( Artifact.class ), any(), any() ) ).then(
                invocation ->
                {
                    Artifact artifact = invocation.getArgument( 0 );
                    if ( "dependency-artifact".equals( artifact.getArtifactId() ) )
                    {
                        return Arrays.asList( new DefaultArtifactVersion( "1.1.1-SNAPSHOT" ),
                                new DefaultArtifactVersion( "1.1.0" ), new DefaultArtifactVersion( "1.1.0-SNAPSHOT" ),
                                new DefaultArtifactVersion( "1.0.0" ), new DefaultArtifactVersion( "1.0.0-SNAPSHOT" ),
                                new DefaultArtifactVersion( "0.9.0" ) );
                    }
                    else if ( "poison-artifact".equals( artifact.getArtifactId() ) )
                    {
                        return Arrays.asList( new DefaultArtifactVersion( "1.1.1.1-SNAPSHOT" ),
                                new DefaultArtifactVersion( "1.1.1.0" ),
                                new DefaultArtifactVersion( "1.1.1.0-SNAPSHOT" ),
                                new DefaultArtifactVersion( "1.0.0.0" ),
                                new DefaultArtifactVersion( "1.0.0.0-SNAPSHOT" ),
                                new DefaultArtifactVersion( "0.9.0.0" ) );
                    }
                    fail();
                    return null;
                } );

        mojo = new UseLatestVersionsMojo()
        {{
            MavenProject project = new MavenProject()
            {{
                setModel( new Model()
                {{
                    setGroupId( "default-group" );
                    setArtifactId( "project-artifact" );
                    setVersion( "1.0.0-SNAPSHOT" );

                    setDependencies( Collections.singletonList(
                            DependencyBuilder.dependencyWith( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT",
                                    "default", "pom", SCOPE_COMPILE ) ) );

                    setDependencyManagement( new DependencyManagement() );
                    getDependencyManagement().setDependencies( Collections.singletonList(
                            DependencyBuilder.dependencyWith( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT",
                                    "default", "pom", SCOPE_COMPILE ) ) );
                }} );
            }};
            setProject( project );
            repositorySystem = repositorySystemMock;
            artifactMetadataSource = artifactMetadataSourceMock;

            changeRecorder = new TestChangeRecorder();
            setVariableValueToObject( this, "changeRecorder", changeRecorder );
        }};
    }

    @Test
    public void testDependenciesDowngradeIncremental()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException
    {
        setVariableValueToObject( mojo, "processDependencies", true );
        setVariableValueToObject( mojo, "allowSnapshots", false );
        setVariableValueToObject( mojo, "allowMajorUpdates", false );
        setVariableValueToObject( mojo, "allowMinorUpdates", true );
        setVariableValueToObject( mojo, "allowIncrementalUpdates", true );
        setVariableValueToObject( mojo, "allowDowngrade", true );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setDependencyVersion( any(), any(), any(), any(), any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0" ) ) );
    }

    @Test
    public void testDependenciesDowngradeMinor()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException
    {
        setVariableValueToObject( mojo, "processDependencies", true );
        setVariableValueToObject( mojo, "allowSnapshots", false );
        setVariableValueToObject( mojo, "allowMajorUpdates", false );
        setVariableValueToObject( mojo, "allowMinorUpdates", true );
        setVariableValueToObject( mojo, "allowIncrementalUpdates", false );
        setVariableValueToObject( mojo, "allowDowngrade", true );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setDependencyVersion( any(), any(), any(), any(), any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.0.0" ) ) );
    }

    @Test
    public void testDependenciesDowngradeMajor()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException
    {
        setVariableValueToObject( mojo, "processDependencies", true );
        setVariableValueToObject( mojo, "allowSnapshots", false );
        setVariableValueToObject( mojo, "allowMajorUpdates", true );
        setVariableValueToObject( mojo, "allowMinorUpdates", false );
        setVariableValueToObject( mojo, "allowIncrementalUpdates", false );
        setVariableValueToObject( mojo, "allowDowngrade", true );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setDependencyVersion( any(), any(), any(), any(), any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "0.9.0" ) ) );
    }

    @Test
    public void testDependencyManagementDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException
    {
        setVariableValueToObject( mojo, "processDependencyManagement", true );
        setVariableValueToObject( mojo, "allowSnapshots", false );
        setVariableValueToObject( mojo, "allowMajorUpdates", false );
        setVariableValueToObject( mojo, "allowMinorUpdates", true );
        setVariableValueToObject( mojo, "allowIncrementalUpdates", true );
        setVariableValueToObject( mojo, "allowDowngrade", true );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.getRawModel( any( MavenProject.class ) ) )
                    .thenReturn( mojo.getProject().getModel() );
            pomHelper.when( () -> PomHelper.setDependencyVersion( any(), any(), any(), any(), any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0" ) ) );
    }

    @Test
    public void testParentDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException
    {
        setVariableValueToObject( mojo, "processParent", true );
        setVariableValueToObject( mojo, "allowSnapshots", false );
        setVariableValueToObject( mojo, "allowMajorUpdates", false );
        setVariableValueToObject( mojo, "allowMinorUpdates", true );
        setVariableValueToObject( mojo, "allowIncrementalUpdates", true );
        setVariableValueToObject( mojo, "allowDowngrade", true );

        mojo.getProject().setParentArtifact(
                new DefaultArtifact( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "compile", "pom",
                        "default", null ) );
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( mojo.getProject().getParentArtifact().getGroupId() );
            setArtifactId( mojo.getProject().getParentArtifact().getArtifactId() );
            setVersion( mojo.getProject().getParentArtifact().getVersion() );
        }} );


        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setDependencyVersion( any(), any(), any(), any(), any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0" ) ) );
    }

    @Test
    public void testPoisonDependencyVersion()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException
    {
        mojo.getProject().getModel().setDependencies( Arrays.asList(
                DependencyBuilder.dependencyWith( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT",
                        "default", "pom", SCOPE_COMPILE ),
                DependencyBuilder.dependencyWith( "default-group", "poison-artifact", "1.1.1.1-SNAPSHOT",
                        "default", "pom", SCOPE_COMPILE )
                ) );

        setVariableValueToObject( mojo, "processDependencies", true );
        setVariableValueToObject( mojo, "allowSnapshots", false );
        setVariableValueToObject( mojo, "allowMajorUpdates", false );
        setVariableValueToObject( mojo, "allowMinorUpdates", true );
        setVariableValueToObject( mojo, "allowIncrementalUpdates", false );
        setVariableValueToObject( mojo, "allowDowngrade", true );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setDependencyVersion( any(), any(), any(), any(), any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        // So, the regular update should take place despite an irregular, or — if I may — "poison", dependency
        // being present in the dependency list
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "dependency-artifact", "1.1.1-SNAPSHOT",
                        "1.0.0" ) ) );
    }

}