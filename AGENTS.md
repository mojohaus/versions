# Agent guide for versions

This file is for automated agents and human contributors working on this repository. It records
what the code and the POMs do not make obvious.

## Modules

|                  Module                   |                                                Contents                                                 |
|-------------------------------------------|---------------------------------------------------------------------------------------------------------|
| `versions-maven-plugin`                   | The goals and reports, their unit tests and the integration tests                                       |
| `versions-common`                         | Version lookup, version rules and POM rewriting shared by the goals                                     |
| `versions-api`                            | Change recording API (`ChangeRecorder`, `VersionChange`)                                                |
| `versions-model`, `versions-model-report` | Modello models for the rules file and the XML reports; edit the `.mdo` files, not the generated classes |
| `versions-enforcer`                       | Maven Enforcer rules, with their own integration tests                                                  |
| `versions-test`                           | Test utilities shared by the other modules, such as `TextAssertions`                                    |

## Build gates

- `mvn install` runs Checkstyle, Spotless and animal-sniffer. Run `mvn spotless:apply` before
  committing; it formats Java with palantir-java-format and also reformats Markdown, including
  tables in this file and the README.
- Main code must use the Java 8 API only: animal-sniffer checks it against the `java18`
  signature, and CI builds on JDK 8 as well as JDK 17, 21 and 25.
- The plugin compiles against Maven 3.9.16 and resolver 1.9.27 and keeps
  `<prerequisites><maven>3.6.3</maven>`. CI runs it on Maven 3.10.0 and 4.0.0-rc-7.
- Protected methods of the mojo base classes, such as `AbstractVersionsUpdaterMojo`, are API for
  subclasses in other plugins. Deprecate them; remove them only in a major version.
- The site is built from `src/site/markdown`; `.md.vm` files go through Velocity. Public classes
  in `versions-test` need complete Javadoc, including `@param`, or the site build fails.

## Unit tests

- JUnit 5 only. JUnit 4, the vintage engine and Hamcrest are not on the classpath; use JUnit
  assertions and, for text output, `TextAssertions` from `versions-test`.
- Mojo tests are annotated with `@ProjectMojoTest` and take a `ProjectMojos` parameter.
  `ProjectMojos.lookup(goal, pomDir)` builds the project from the test POM, so parent inheritance
  and plugin configuration apply as in a real build. `ItRepository` offers the versions in
  `src/it-repo`, the repository the integration tests use.
- Prefer a unit test. Add an integration test only for what needs a real Maven build: reactor
  builds, parent resolution, version ranges resolved against a repository, `-f`, or plugin
  prerequisites read from plugin POMs.

## Integration tests

- They live in `versions-maven-plugin/src/it`, one directory per test, and resolve from a mock
  repository serving `src/it-repo`. Each one is a separate Maven run of 2 to 7 seconds, so there
  are about 240 Maven starts per CI job.
- Install the other modules first, then run a subset from the plugin module:

  ```shell
  mvn install -DskipTests -Dinvoker.skip
  cd versions-maven-plugin
  mvn -P run-its verify -Dinvoker.test='it-set-*'
  ```

  `-Dinvoker.test` filters the `setup-*` projects too. List the ones your tests need, such as
  `setup-provide-rules-in-jar` for `it-rules-via-classpath-*`.

- They run in parallel, one per CPU core, so a test must not depend on another test's output.

- The forked builds get `JAVA_TOOL_OPTIONS=-XX:TieredStopAtLevel=1 -XX:+UseSerialGC`, so every JVM
  prints a `Picked up JAVA_TOOL_OPTIONS` line on stderr. Maven 4.0.0-rc-7 does not start at all
  when that variable is set and the project has `.mvn/jvm.config`.

- Write `verify.groovy` as `assert` statements. Invoker fails a script whose last expression is
  anything other than `true` or `null`, so a trailing `list.each { }` fails the test; use a `for`
  loop.

- Restrict a test to some Maven versions with `invoker.maven.version` in `invoker.properties`.

## Fixtures and dependencies

- POMs under `src/it`, `src/it-repo` and `src/test/resources` pin old versions on purpose,
  including some with known vulnerabilities. Do not update them. Dependabot excludes `src/it` and
  `src/test/resources` from version updates, but security updates can still propose them; close
  those pull requests.
- Deliberate pins in `.github/dependabot.yml`: Mockito stays below 5 because the build runs on
  JDK 8, and plexus-xml below 4 because 4.x depends on the Maven 4 XML API.

## Pull requests

- One issue per pull request; put `Fixes #123` in the description.
- Pull requests are squash-merged. Release Drafter builds the release notes from the titles and
  labels, so the title should describe the change for a user.

