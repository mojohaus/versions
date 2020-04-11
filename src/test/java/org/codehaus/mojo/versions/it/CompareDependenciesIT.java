package org.codehaus.mojo.versions.it;

import org.apache.maven.jupiter.extension.MavenIT;
import org.apache.maven.jupiter.extension.MavenTest;
import org.apache.maven.jupiter.maven.MavenExecutionResult;
import org.apache.maven.jupiter.maven.MavenProjectResult;

import static org.apache.maven.assertj.MavenITAssertions.assertThat;

@MavenIT
class CompareDependenciesIT
{

    @MavenTest( goals = {"${project.groupId}:${project.artifactId}:${project.version}:compare-dependencies"},
                systemProperties = {"remotePom=localhost:dummy-bom-pom:1.0", "reportOutputFile=target/depDiffs.txt"} )
    void it_compare_dependencies_001( MavenExecutionResult result, MavenProjectResult mavenProjectResult )
    {
        assertThat( result ).isSuccessful()
                .project()
                .hasTarget()
                .withFile( "depDiffs.txt" )
                .hasContent( String.join( "\n", "The following differences were found:", "", "  none", "",
                        "The following property differences were found:", "", "  none" ) );
    }


}
