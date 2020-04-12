package org.codehaus.mojo.versions.it;

import org.apache.maven.jupiter.extension.MavenIT;
import org.apache.maven.jupiter.extension.MavenProject;
import org.apache.maven.jupiter.extension.MavenRepository;
import org.apache.maven.jupiter.extension.MavenTest;
import org.apache.maven.jupiter.maven.MavenExecutionResult;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;

import static org.apache.maven.assertj.MavenExecutionResultAssert.assertThat;

@TestMethodOrder( MethodOrderer.OrderAnnotation.class)
@MavenIT
class UpdateChildModulesIT
{
    @Nested
    @MavenRepository
    @MavenProject
    class One {
        @MavenTest( options = {"-o"}, goals = { "validate" })
        @Order( 10)
        void first_test( MavenExecutionResult result) {
            assertThat(result).isSuccessful();
        }

        @MavenTest(options = {"-o"}, goals = { "validate" })
        @Order(20)
        @DisplayName( "where setup two is needed.")
        void second_test(MavenExecutionResult result) {
            assertThat(result).isFailure();
        }

        @MavenTest(options = {"-N"}, goals = { "${project.groupId}:${project.artifactId}:${project.version}:update-child-modules" })
        @Order(30)
        @DisplayName("where setup two is needed.")
        void third_test(MavenExecutionResult result) {
            assertThat(result).isSuccessful();
        }

        @MavenTest(goals = { "validate" })
        @Order(10)
        void forth_test(MavenExecutionResult result) {
            assertThat(result).isSuccessful();
        }
    }

}
