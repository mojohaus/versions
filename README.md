# MojoHaus Versions Maven Plugin

[![Apache License, Version 2.0, January 2004](https://img.shields.io/github/license/mojohaus/versions.svg?label=License)](https://www.apache.org/licenses/LICENSE-2.0)
[![Maven Central](https://img.shields.io/maven-central/v/org.codehaus.mojo/versions-maven-plugin.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/org.codehaus.mojo/versions-maven-plugin)
[![Build Status](https://github.com/mojohaus/versions/actions/workflows/maven.yml/badge.svg?branch=master)](https://github.com/mojohaus/versions/actions/workflows/maven.yml?query=branch%3Amaster)

The Versions Maven Plugin finds newer versions of the dependencies, plugins, parents and
properties in your POMs, reports them, and updates the POMs for you.

The documentation is on the [project site](https://www.mojohaus.org/versions/versions-maven-plugin/):
[usage](https://www.mojohaus.org/versions/versions-maven-plugin/usage.html),
[goals](https://www.mojohaus.org/versions/versions-maven-plugin/plugin-info.html),
[version rules](https://www.mojohaus.org/versions/versions-maven-plugin/version-rules.html) and
[FAQ](https://www.mojohaus.org/versions/versions-maven-plugin/faq.html).

## Quick start

The plugin runs from the command line; nothing needs to be added to the POM.

```shell
# Show newer versions of dependencies, plugins and version properties
mvn versions:display-dependency-updates
mvn versions:display-plugin-updates
mvn versions:display-property-updates

# Update the POM
mvn versions:use-latest-releases      # dependencies to their latest releases
mvn versions:update-properties        # version properties
mvn versions:update-parent            # the parent version
mvn versions:set -DnewVersion=1.2.0   # the project's own version, across the reactor

# Keep or discard the POM backups the update goals write
mvn versions:commit
mvn versions:revert
```

To leave out pre-releases such as alphas, betas and release candidates, use
[version rules](https://www.mojohaus.org/versions/versions-maven-plugin/version-rules.html)
or `-Dmaven.version.ignore`.

## Requirements

The plugin runs on Maven 3.6.3 or later and JDK 8 or later. We support the latest plugin version
with the latest Maven release, and CI tests it on Linux, macOS and Windows with Maven 3 and Maven 4.

## Reporting a problem

1. Search the [open and closed issues](https://github.com/mojohaus/versions/issues?q=is%3Aissue)
   and pull requests first; the problem may already be reported or fixed.
2. Check that it still happens with the latest plugin and the latest Maven.
3. [Open an issue](https://github.com/mojohaus/versions/issues/new) with the plugin, Maven and JDK
   versions, what you expected, and what happened. A small POM or project that reproduces it,
   and the output with `-X`, help the most.

Report one problem per issue.

## Contributing

For a larger change or a new feature, open an issue first or write to the
[mailing list](https://www.mojohaus.org/versions/mailing-lists.html), and wait for feedback
before you start.

A pull request addresses one issue. We squash-merge it, and the pull request title becomes the
commit subject in the Git history and the release notes, so write the title as a description of
the change, and put `Fixes #123` in the description to close the issue.

### Modules

|                  Module                   |                              Contents                               |
|-------------------------------------------|---------------------------------------------------------------------|
| `versions-maven-plugin`                   | The plugin: goals, reports and their tests                          |
| `versions-common`                         | Version lookup, version rules and POM rewriting shared by the goals |
| `versions-api`                            | Extension API, such as the change recorder                          |
| `versions-model`, `versions-model-report` | Modello models for the rules file and the XML reports               |
| `versions-enforcer`                       | Maven Enforcer rules built on the plugin                            |
| `versions-test`                           | Test utilities used by the other modules                            |

### Building and testing

```shell
mvn install                         # build, unit tests, Checkstyle and Spotless
mvn spotless:apply                  # format the code before you commit
cd versions-maven-plugin
mvn -P run-its verify               # integration tests (each one is a separate Maven run)
mvn -P run-its verify -Dinvoker.test='it-set-*'   # only the matching integration tests
```

Unit tests use JUnit 5. A mojo test annotated with `@ProjectMojoTest` gets the project built from
its test POM and looks the mojo up through `ProjectMojos`; see the existing tests in
`versions-maven-plugin/src/test`.

Integration tests live in `versions-maven-plugin/src/it`, one directory per test, and resolve
artifacts from the test repository in `src/it-repo`. Write `verify.groovy` with `assert`
statements, so a failure shows the expression that failed. Add an integration test when the
behaviour needs a real Maven build, such as reactor builds, parent resolution or version ranges;
otherwise, a unit test is faster and gives a clearer failure.

## Releasing

* Make sure `gpg-agent` is running.
* Run `mvn -B release:prepare release:perform`.

To publish the site:

```shell
cd target/checkout
mvn site
mvn scm-publish:publish-scm
```

For a multi-module site, two executions are needed.
