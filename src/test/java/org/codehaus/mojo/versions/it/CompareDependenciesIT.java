package org.codehaus.mojo.versions.it;

import org.apache.maven.jupiter.extension.MavenIT;
import org.apache.maven.jupiter.extension.MavenTest;
import org.apache.maven.jupiter.maven.MavenExecutionResult;

import static org.apache.maven.assertj.MavenITAssertions.assertThat;

@MavenIT
class CompareDependenciesIT {

  @MavenTest(goals = {"org.codehaus.mojo:versions-maven-plugin:${project.version}:compare-dependencies"},
    systemProperties = {"remotePom=localhost:dummy-bom-pom:1.0", "reportOutputFile=target/depDiffs.txt"})
  void it_compare_dependencies_001(MavenExecutionResult result) {
    assertThat(result).isSuccessful();
//    assertThat(project).hasTarget()
//      .withEarFile()
//      .containsOnlyOnce("META-INF/application.xml", "META-INF/appserver-application.xml");
  }


}
