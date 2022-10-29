package org.codehaus.mojo.versions;

import java.util.Objects;

import org.apache.maven.model.Dependency;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Hamcrest-Matcher that matches a {@link Dependency} GAV
 */
public class HasGAVMatcher extends TypeSafeMatcher<Dependency>
{
    private final String groupId;
    private final String artifactId;
    private final String version;

    public HasGAVMatcher( String groupId, String artifactId, String version )
    {
        this.groupId = groupId;
        this.artifactId = artifactId;
        this.version = version;
    }

    public static HasGAVMatcher hasGAVOf( Dependency dependency )
    {
        return hasGAV( dependency.getGroupId(), dependency.getArtifactId(), dependency.getVersion() );
    }

    public static HasGAVMatcher hasGAV( String groupId, String artifactId, String version )
    {
        return new HasGAVMatcher( groupId, artifactId, version );
    }

    @Override
    protected boolean matchesSafely( Dependency item )
    {
        boolean result = Objects.equals( groupId, item.getGroupId() )
                && Objects.equals( artifactId, item.getArtifactId() )
                && Objects.equals( version, item.getVersion() );

        return result;
    }

    @Override
    public void describeTo( Description description )
    {
        description.appendText( String.format( "has GAV %s:%s:%s", groupId, artifactId, version ) );
    }
}
