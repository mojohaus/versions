package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultVersionChange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.MockedStatic;

import static java.util.Collections.singleton;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

public class UpdateParentMojoTest
{
    private TestChangeRecorder changeRecorder;

    private UpdateParentMojo mojo;

    private static RepositorySystem repositorySystem;

    private static org.eclipse.aether.RepositorySystem aetherRepositorySystem;

    @BeforeClass
    public static void setUpStatic()
    {
        repositorySystem = mockRepositorySystem();
        aetherRepositorySystem = mockAetherRepositorySystem( new HashMap<String, String[]>()
        {{
            put( "parent-artifact", new String[] { "0.9.0", "1.0.0", "1.0.1-SNAPSHOT" } );
            put( "issue-670-artifact", new String[] { "0.0.1-1", "0.0.1-1-impl-SNAPSHOT" } );
            put( "dummy-parent2", new String[] { "1.0", "2.0", "3.0", "3.0-alpha-1", "3.0-beta-1" } );
            put( "test-incremental", new String[] { "1.0.0", "1.1.0", "1.1.1", "2.0.0" } );
            put( "unknown-artifact", new String[0] );
        }} );
    }

    @Before
    public void setUp() throws IllegalAccessException
    {
        changeRecorder = new TestChangeRecorder();

        mojo = new UpdateParentMojo( repositorySystem, aetherRepositorySystem, null, changeRecorder.asTestMap() )
        {{
            setProject( createProject() );
            reactorProjects = Collections.emptyList();
            session = mockMavenSession();
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
                    ? dependency.getClassifier() : "default",
                    new DefaultArtifactHandlerStub( "default" ) );
        } );
        return repositorySystem;
    }

    @Test
    @SuppressWarnings( "deprecation" )
    public void testArtifactIdDoesNotExist()
            throws VersionRetrievalException, MojoExecutionException,
            XMLStreamException, MojoFailureException, InvalidVersionSpecificationException,
            VersionRetrievalException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "unknown-artifact" );
            setVersion( "1.0.1-SNAPSHOT" );
        }} );

        Artifact artifact =
                new DefaultArtifact( "default-group", "unknown-artifact", "1.0.1-SNAPSHOT", SCOPE_COMPILE, "pom",
                        "default", new DefaultArtifactHandlerStub( "default" ) );
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
            throws MojoExecutionException, XMLStreamException, MojoFailureException,
            VersionRetrievalException
    {
        mojo.allowDowngrade = true;
        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new DefaultVersionChange( "default-group", "parent-artifact", "1.0.1-SNAPSHOT",
                                                   "1.0.0" ) ) );
    }

    @Test
    public void testParentDowngradeForbidden()
            throws MojoExecutionException, XMLStreamException, MojoFailureException,
            VersionRetrievalException
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
            throws MojoExecutionException, VersionRetrievalException,
            InvalidVersionSpecificationException, InvalidSegmentException
    {
        mojo.allowDowngrade = true;
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
        }} );

        ArtifactVersion newVersion = mojo.resolveTargetVersion( "[1.0.1-SNAPSHOT,)" );
        assertThat( newVersion, notNullValue() );
        assertThat( newVersion.toString(), is( "1.0.0" ) );
    }

    @Test
    public void testParentDowngradeForbiddenWithRange()
            throws MojoExecutionException, VersionRetrievalException,
            InvalidVersionSpecificationException, InvalidSegmentException
    {
        mojo.allowDowngrade = false;
        ArtifactVersion newVersion = mojo.resolveTargetVersion( "[1.0.1-SNAPSHOT,)" );
        assertThat( newVersion, nullValue() );
    }

    @Test
    public void testAllowSnapshots()
            throws MojoExecutionException, VersionRetrievalException,
            InvalidVersionSpecificationException, InvalidSegmentException
    {
        mojo.allowSnapshots = true;
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "issue-670-artifact" );
        }} );

        ArtifactVersion newVersion = mojo.resolveTargetVersion( "0.0.1-1" );
        assertThat( newVersion, notNullValue() );
        assertThat( newVersion.toString(), is( "0.0.1-1-impl-SNAPSHOT" ) );
    }

    @Test
    public void testAllowSnapshotsWithParentVersion()
            throws MojoExecutionException, XMLStreamException, MojoFailureException,
            VersionRetrievalException
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
        assertThat( changeRecorder.getChanges(), hasItem( new DefaultVersionChange( "default-group",
                                                                                    "issue-670-artifact", "0.0.1-1",
                                                                                    "0.0.1-1-impl-SNAPSHOT" ) ) );
    }

    @Test
    public void testIgnoredVersions()
            throws MojoExecutionException, IllegalAccessException,
            VersionRetrievalException, InvalidVersionSpecificationException, InvalidSegmentException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
        }} );
        setVariableValueToObject( mojo, "ignoredVersions", singleton( "1.0.0" ) );
        assertThat( mojo.resolveTargetVersion( "0.9.0" ), nullValue() );
    }

    @Test
    public void testSkipResolutionDowngradeUnknownVersion() throws VersionRetrievalException
    {
        testSkipResolution( "0.8.0" );
    }

    @Test
    public void testSkipResolutionDowngrade() throws VersionRetrievalException
    {
        testSkipResolution( "0.9.0" );
    }

    @Test
    public void testSkipResolutionUpgradeUnknownVersion() throws VersionRetrievalException
    {
        testSkipResolution( "2.0.0" );
    }

    private void testSkipResolution( String version ) throws VersionRetrievalException
    {
        mojo.parentVersion = version;
        mojo.skipResolution = true;
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
            setVersion( "1.0.0" );
        }} );

        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) )
                    .thenReturn( true );
            mojo.update( null );
        }
        catch ( MojoExecutionException | XMLStreamException | MojoFailureException e )
        {
            throw new RuntimeException( e );
        }

        assertThat( changeRecorder.getChanges(), hasItem(
            new DefaultVersionChange( "default-group", "parent-artifact", "1.0.0", version ) ) );
    }

    @Test
    public void testShouldUpgradeToSnapshot() throws MojoExecutionException, XMLStreamException, MojoFailureException,
            VersionRetrievalException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
            setVersion( "0.9.0" );
        }} );
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[0,1.0.1-SNAPSHOT]";
        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) ).thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new DefaultVersionChange( "default-group", "parent-artifact", "0.9.0",
                                                   "1.0.1-SNAPSHOT" ) ) );
    }

    @Test
    public void testAllowMinorUpdates()
            throws MojoExecutionException, VersionRetrievalException,
            InvalidVersionSpecificationException, InvalidSegmentException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "parent-artifact" );
            setVersion( "0.8.0" );
        }} );
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = true;
        mojo.allowIncrementalUpdates = true;

        ArtifactVersion newVersion = mojo.resolveTargetVersion( "0.8.0" );

        assertThat( newVersion, notNullValue() );
        assertThat( newVersion.toString(), is( "0.9.0" ) );
    }

    @Test
    public void testAllowIncrementalUpdates()
            throws MojoExecutionException, VersionRetrievalException,
            InvalidVersionSpecificationException, InvalidSegmentException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "test-incremental" );
        }} );
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = false;
        mojo.allowIncrementalUpdates = true;

        ArtifactVersion newVersion = mojo.resolveTargetVersion( "1.1.0" );

        assertThat( newVersion, notNullValue() );
        assertThat( newVersion.toString(), is( "1.1.1" ) );
    }

    @Test
    public void testParentVersionRange() throws MojoExecutionException, XMLStreamException, MojoFailureException,
            VersionRetrievalException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "dummy-parent2" );
            setVersion( "1.0" );
        }} );
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[,3.0-!)";
        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) ).thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(),
                hasItem( new DefaultVersionChange( "default-group", "dummy-parent2", "1.0",
                                                   "2.0" ) ) );
    }

    @Test
    public void testParentVersionRange2() throws MojoExecutionException, XMLStreamException, MojoFailureException,
            VersionRetrievalException
    {
        mojo.getProject().setParent( new MavenProject()
        {{
            setGroupId( "default-group" );
            setArtifactId( "dummy-parent2" );
            setVersion( "2.0" );
        }} );
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[,3.0-!)";
        try ( MockedStatic<PomHelper> pomHelper = mockStatic( PomHelper.class ) )
        {
            pomHelper.when( () -> PomHelper.setProjectParentVersion( any(), any() ) ).thenReturn( true );
            mojo.update( null );
        }
        assertThat( changeRecorder.getChanges(), empty() );
    }
}
