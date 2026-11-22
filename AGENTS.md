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
  signature, and CI builds on JDK 8 as well as newer JDKs.
- The plugin compiles against the Maven 3.9.x and Resolver 1.9.x line (`mavenVersion` in the root
  POM; Dependabot ignores the 3.10 and 2.x releases) so that it keeps running on Maven 3.9.0, the
  `<prerequisites><maven>` of the plugin. `.github/workflows/maven.yml` calls the shared build
  workflow, which also runs the plugin on Maven 3.10 and on Maven 4.
- Protected methods of the mojo base classes, such as `AbstractVersionsUpdaterMojo`, are API for
  subclasses in other plugins. Deprecate them; remove them only in a major version.
- The site is built from the `src/site/markdown` directory of each module; `.md.vm` files go
  through Velocity. Public classes in `versions-test` need complete Javadoc, including `@param`,
  or the site build fails.

## Unit tests

- Unit tests use JUnit 5. Use JUnit assertions and, for text output, `TextAssertions` from
  `versions-test`.
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

## Documentation updates

A change that users or contributors can notice ships with the documentation it affects, in the
same pull request. A refactor with no visible effect needs none.

- A goal or parameter is documented in its Javadoc, which becomes the goal page
  (`plugin-info.html` lists the goals). Give a new `@Parameter` a description, its default and
  `@since`.
- `versions-maven-plugin/src/site/markdown` holds the overview (`index.md`, which lists every
  goal), `usage.md.vm`, `faq.md`, `version-rules.md.vm` and the pages under `examples/`. Add a
  page under `examples/` for a new behaviour that needs more than a parameter description.
- `versions-api/src/site/markdown` documents the extension API, and
  `versions-enforcer/src/site/markdown/index.md` the `maxDependencyUpdates` rule and its
  parameters.
- Keep the `site.xml` of the module in sync when a page is added or removed. Never rename a
  published page or a heading: both are URLs that readers link to.
- Pages that use Velocity (`#set`, `#parse`, `$h2`) stay `.md.vm`; write every other page as
  plain `.md`. Examples show the plugin without a version, or with
  `<version><!-- latest release --></version>` where a version is needed, and never a snapshot
  version unless snapshots are the subject of the page.
- `README.md` covers what the plugin does, its Maven and JDK floor, and how to contribute; update
  it when those change. Update this file when a change adds a convention, build gate or fixture.

## Pull requests

- One issue per pull request; put `Fixes #123` in the description.
- Pull requests are squash-merged. Release Drafter builds the release notes from the titles and
  labels, so the title should describe the change for a user.

