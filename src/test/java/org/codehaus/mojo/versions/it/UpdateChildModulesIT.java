package org.codehaus.mojo.versions.it;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;

import com.soebes.itf.jupiter.extension.MavenIT;
import com.soebes.itf.jupiter.extension.MavenProject;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenExecutionResult;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder( MethodOrderer.OrderAnnotation.class)
@MavenIT
class UpdateChildModulesIT
{
    @Nested
    @MavenRepository
    @MavenProject
    class One {
        @MavenTest(options = {"-o"}, goals = { "validate" })
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
