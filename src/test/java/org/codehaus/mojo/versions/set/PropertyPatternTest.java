package org.codehaus.mojo.versions.set;

import org.junit.Assert;

import junit.framework.TestCase;

public class PropertyPatternTest
    extends TestCase
{
    public void testAnyLiteral()
    {
        PropertyPattern p = PropertyPattern.of( "myProperty" );
        Assert.assertTrue( p.toString() + " should match profile-less myProperty",
                           p.matches( PropertyScope.profileLess(), "myProperty" ) );
        Assert.assertTrue( p.toString() + " should match myProperty under anyProfile",
                           p.matches( PropertyScope.ofProfile( "anyProfile" ), "myProperty" ) );
    }

    public void testProfileLessLiteral()
    {
        PropertyPattern p = PropertyPattern.of( "/myProperty" );
        Assert.assertTrue( p.toString() + " should match profile-less myProperty",
                           p.matches( PropertyScope.profileLess(), "myProperty" ) );
        Assert.assertFalse( p.toString() + " should not match myProperty under anyProfile",
                           p.matches( PropertyScope.ofProfile( "anyProfile" ), "myProperty" ) );
    }

    public void testInProfileLiteral()
    {
        PropertyPattern p = PropertyPattern.of( "myProfile/myProperty" );
        Assert.assertFalse( p.toString() + " should not match profile-less myProperty",
                           p.matches( PropertyScope.profileLess(), "myProperty" ) );
        Assert.assertTrue( p.toString() + " should match myProperty under myProfile",
                           p.matches( PropertyScope.ofProfile( "myProfile" ), "myProperty" ) );
        Assert.assertFalse( p.toString() + " should not match myProperty under anyProfile",
                            p.matches( PropertyScope.ofProfile( "anyProfile" ), "myProperty" ) );
    }

    public void testAnyPattern()
    {
        PropertyPattern p = PropertyPattern.of( "myProperty*" );
        Assert.assertTrue( p.toString() + " should match profile-less myProperty",
                           p.matches( PropertyScope.profileLess(), "myProperty" ) );
        Assert.assertTrue( p.toString() + " should match profile-less myProperty42",
                           p.matches( PropertyScope.profileLess(), "myProperty42" ) );
        Assert.assertTrue( p.toString() + " should match myProperty under anyProfile",
                           p.matches( PropertyScope.ofProfile( "anyProfile" ), "myProperty" ) );
        Assert.assertTrue( p.toString() + " should match myProperty42 under anyProfile",
                           p.matches( PropertyScope.ofProfile( "anyProfile" ), "myProperty42" ) );
    }

    public void testProfilePattern()
    {
        PropertyPattern p = PropertyPattern.of( "*/myProperty" );
        Assert.assertFalse( p.toString() + " should not match profile-less myProperty",
                           p.matches( PropertyScope.profileLess(), "myProperty" ) );
        Assert.assertTrue( p.toString() + " should match myProperty under myProfile",
                           p.matches( PropertyScope.ofProfile( "myProfile" ), "myProperty" ) );
        Assert.assertTrue( p.toString() + " should not match myProperty under anyProfile",
                            p.matches( PropertyScope.ofProfile( "anyProfile" ), "myProperty" ) );
    }
}
