package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import java.util.Arrays;
import java.util.Collections;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.change.VersionChange;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.MockedStatic;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

public class UpdateParentMojoTest
{
    private TestChangeRecorder changeRecorder;

    private UpdateParentMojo mojo;

    private ArtifactResolver artifactResolver;

    private static RepositorySystem repositorySystem;

    @SuppressWarnings( "deprecation" )
    private static ArtifactMetadataSource artifactMetadataSource;

    @BeforeClass
    @SuppressWarnings( "deprecation" )
    public static void setUpStatic() throws ArtifactMetadataRetrievalException
    {
        repositorySystem = mockRepositorySystem();
        artifactMetadataSource = mockArtifactMetaDataSource();
    }

    @Before
    public void setUp() throws IllegalAccessException
    {
        changeRecorder = new TestChangeRecorder();
        artifactResolver = mock( ArtifactResolver.class );

        mojo = new UpdateParentMojo()
        {{
            setProject( createProject() );
            reactorProjects = Collections.emptyList();
            repositorySystem = UpdateParentMojoTest.repositorySystem;
            artifactMetadataSource = UpdateParentMojoTest.artifactMetadataSource;
            resolver = UpdateParentMojoTest.this.artifactResolver;

            setVariableValueToObject( this, "changeRecorder", changeRecorder );
        }};
    }

    private MavenProject createProject()
    {
        return new MavenProject()
        {{
            setModel( new Model()
            {{
                setGroupId( "default-group" );
                setArtifactId( "project-artifact" );
                setVersion( "1.0.1-SNAPSHOT" );
            }} );

            setParent( new MavenProject()
            {{
                setGroupId( "default-group" );
                setArtifactId( "parent-artifact" );
                setVersion( "1.0.1-SNAPSHOT" );
            }} );
        }};
    }

    private static RepositorySystem mockRepositorySystem()
    {
        RepositorySystem repositorySystem = mock( RepositorySystem.class );
        when( repositorySystem.createDependencyArtifact( any( Dependency.class ) ) ).thenAnswer( invocation ->
        {
            Dependency dependency = invocation.getArgument( 0 );
            return new DefaultArtifact( dependency.getGroupId(), dependency.getArtifactId(), dependency.getVersion(),
                    dependency.getScope(), dependency.getType(), dependency.getClassifier() != null
                    ? dependency.getClassifier() : "default", null );
        } );
        return repositorySystem;
    }

    @SuppressWarnings( "deprecation" )
    private static ArtifactMetadataSource mockArtifactMetaDataSource() throws ArtifactMetadataRetrievalException
    {
        ArtifactMetadataSource artifactMetadataSource = mock( ArtifactMetadataSource.class );
        when( artifactMetadataSource.retrieveAvailableVersions( any( Artifact.class ), any(), any() ) ).then(
                invocation ->
                {
                    Artifact artifact = invocation.getArgument( 0 );
                    if ( "parent-artifact".equals( artifact.getArtifactId() ) )
                    {
                        return Arrays.asList( new DefaultArtifactVersion( "1.0.1-SNAPSHOT" ),
                                new DefaultArtifactVersion( "1.0.0" ),
                                new DefaultArtifactVersion( "0.9.0" ) );
                    }
                    else if ( "issue-670-artifact".equals( artifact.getArtifactId() ) )
                    {
                        return Arrays.asList( new DefaultArtifactVersion( "0.0.1-1" ),
                                new DefaultArtifactVersion( "0.0.1-1-impl-SNAPSHOT" ) );
                    }
                    else if ( "unknown-artifact".equals( artifact.getArtifactId() ) )
                    {
                        return Collections.emptyList();
                    }
                    fail();
                    return null;
                } );
        return artifactMetadataSource;
    }

    @Test
    @SuppressWarnings( "deprecation" )
    public void testArtifactIdDoesNotExist()
            throws ArtifactMetadataRetrievalException, MojoExecutionException,
            XMLStreamException, MojoFailureException, InvalidVersionSpecificationException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "unknown-artifact" );
            setVersion( "1.0.1-SNAPSHOT" );
        }} );

        Artifact artifact =
                new DefaultArtifact( "default-group", "unknown-artifact", "1.0.1-SNAPSHOT", SCOPE_COMPILE, "pom",
                        "default", null );
        assertThat(
                mojo.findLatestVersion( artifact, VersionRange.createFromVersionSpec( "1.0.1-SNAPSHOT" ), null, false ),
                is( nullValue() ) );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) ).thenReturn( true );
            mojo.update( null );
        }
    }

    @Test
    public void testParentDowngradeAllowed()
            throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        mojo.allowDowngrade = true;
        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "parent-artifact", "1.0.1-SNAPSHOT",
                        "1.0.0" ) ) );
    }

    @Test
    public void testParentDowngradeForbidden()
            throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        mojo.allowDowngrade = false;
        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(), is( empty() ) );
    }

    @Test
    public void testParentDowngradeAllowedWithRange()
            throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        mojo.allowDowngrade = true;
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
            setVersion( "[1.0.1-SNAPSHOT,)" );
        }} );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new VersionChange( "default-group", "parent-artifact", "[1.0.1-SNAPSHOT,)",
                        "1.0.0" ) ) );
    }

    @Test
    public void testParentDowngradeForbiddenWithRange()
            throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        mojo.allowDowngrade = false;
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
            setVersion( "[1.0.1-SNAPSHOT,)" );
        }} );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(), is( empty() ) );
    }

    @Test
    public void testAllowSnapshots()
            throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        mojo.allowSnapshots = true;
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "issue-670-artifact" );
            setVersion( "0.0.1-1" );
        }} );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(), hasItem( new VersionChange( "default-group",
                "issue-670-artifact", "0.0.1-1",
                "0.0.1-1-impl-SNAPSHOT" ) ) );
    }

    @Test
    public void testAllowSnapshotsWithParentVersion()
            throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        mojo.allowSnapshots = true;
        mojo.parentVersion = "0.0.1-1-impl-SNAPSHOT";
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "issue-670-artifact" );
            setVersion( "0.0.1-1" );
        }} );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(), hasItem( new VersionChange( "default-group",
                "issue-670-artifact", "0.0.1-1",
                "0.0.1-1-impl-SNAPSHOT" ) ) );
    }
}
