package org.codehaus.mojo.versions.it;

import org.apache.maven.jupiter.extension.MavenIT;
import org.apache.maven.jupiter.extension.MavenOptions;
import org.apache.maven.jupiter.extension.MavenTest;
import org.apache.maven.jupiter.maven.MavenExecutionResult;
import org.apache.maven.jupiter.maven.MavenProjectResult;

import static org.apache.maven.assertj.MavenITAssertions.assertThat;

@MavenIT
class CompareDependenciesIT
{

    private static final String VERSIONS_PLUGIN = "${project.groupId}:${project.artifactId}:${project.version}";


    @MavenTest( options = {MavenOptions.SETTINGS, "settings.xml"}, goals={VERSIONS_PLUGIN + ":compare-dependencies"},
                systemProperties = {"remotePom=localhost:dummy-bom-pom:1.0", "reportOutputFile=target/depDiffs.txt"} )
    void it_compare_dependencies_001( MavenExecutionResult result, MavenProjectResult mavenProjectResult )
    {
        assertThat( result ).isSuccessful()
                .project()
                .hasTarget()
                .withFile( "depDiffs.txt" )
                .hasContent( String.join( "\n",
                        "The following differences were found:",
                    "",
                    "  org.apache.maven:maven-artifact ..................... 2.0.10 -> 2.0.9",
                    "",
                    "The following property differences were found:",
                    "",
                    "  none" ) );
    }

    @MavenTest( goals = {VERSIONS_PLUGIN + ":compare-dependencies"},
                systemProperties = {"remotePom=localhost:dummy-bom-pom:1.0", "reportMode=false", "updatePropertyVersions=true"} )
    void it_compare_dependencies_002( MavenExecutionResult result, MavenProjectResult mavenProjectResult )
    {
        assertThat( result ).isSuccessful()
                .project()
                .hasTarget()
                .withFile( "depDiffs.txt" )
                .hasContent( String.join( "\n",
                        "The following differences were found:",
                    "",
                    "  org.apache.maven:maven-artifact ..................... 2.0.10 -> 2.0.9",
                    "",
                    "The following property differences were found:",
                    "",
                    "  none" ) );
    }
    @MavenTest( goals = {VERSIONS_PLUGIN + ":compare-dependencies"},
                systemProperties = {"remotePom=localhost:dummy-bom-maven-mismatch:1.0", "reportMode=false", "updatePropertyVersions=true"} )
    void it_compare_dependencies_003( MavenExecutionResult result, MavenProjectResult mavenProjectResult )
    {
        assertThat( result ).isSuccessful()
                .project()
                .hasTarget()
                .withFile( "depDiffs.txt" )
                .hasContent( String.join( "\n",
                        "The following differences were found:",
                    "",
                    "  org.apache.maven:maven-artifact ..................... 2.0.10 -> 2.0.9",
                    "",
                    "The following property differences were found:",
                    "",
                    "  none" ) );
    }

    @MavenTest( goals = {VERSIONS_PLUGIN + ":compare-dependencies"},
                systemProperties = {
            "remotePom=localhost:dummy-bom-pom:1.0",
            "reportMode=true",
            "reportOutputFile=target/depDiffs.txt",
            "updatePropertyVersions=true"} )
    void it_compare_dependencies_004( MavenExecutionResult result, MavenProjectResult mavenProjectResult )
    {
        assertThat( result ).isSuccessful()
                .project()
                .hasTarget()
                .withFile( "depDiffs.txt" )
                .hasContent( String.join( "\n",
                        "The following differences were found:",
                        "",
                        "  org.apache.maven:maven-artifact ..................... 2.0.10 -> 2.0.9",
                        "  junit:junit .............................................. 4.8 -> 4.1",
                        "",
                        "The following property differences were found:",
                        "",
                        "  junit.version ............................................ 4.8 -> 4.1" ) );
    }

    @MavenTest( goals = {VERSIONS_PLUGIN + ":compare-dependencies"},
                systemProperties = {
            "remotePom=localhost:dummy-bom-pom:1.0",
            "reportMode=true",
            "reportOutputFile=target/depDiffs.txt",
            "updatePropertyVersions=true"} )
    void it_compare_dependencies_005( MavenExecutionResult result, MavenProjectResult mavenProjectResult )
    {
        assertThat( result ).isSuccessful()
                .project()
                .hasTarget()
                .withFile( "depDiffs.txt" )
                .hasContent( String.join( "\n",
        "The following differences were found:",
                    "",
                    "  org.apache.maven:maven-artifact ..................... 2.0.10 -> 2.0.9",
                    "",
                    "The following property differences were found:",
                    "",
                    "  none" ));
    }


}
