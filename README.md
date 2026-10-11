# MojoHaus Versions Maven Plugin

[![Apache License, Version 2.0, January 2004](https://img.shields.io/github/license/mojohaus/versions.svg?label=License)](https://www.apache.org/licenses/LICENSE-2.0)
[![Maven Central](https://img.shields.io/maven-central/v/org.codehaus.mojo/versions-maven-plugin.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/org.codehaus.mojo/versions-maven-plugin)
[![Build Status](https://github.com/mojohaus/versions/actions/workflows/maven.yml/badge.svg?branch=master)](https://github.com/mojohaus/versions/actions/workflows/maven.yml?query=branch%3Amaster)

The Versions Maven Plugin finds newer versions of the dependencies, plugins, parents and
properties in your POMs, reports them, and updates the POMs. It needs Maven 3.6.3 or later and
JDK 8 or later, and nothing in the POM to run from the command line:

```shell
mvn versions:display-dependency-updates
mvn versions:use-latest-releases
mvn versions:set -DnewVersion=1.2.0
```

See the [project site](https://www.mojohaus.org/versions/versions-maven-plugin/) for the
[goals](https://www.mojohaus.org/versions/versions-maven-plugin/plugin-info.html),
[version rules](https://www.mojohaus.org/versions/versions-maven-plugin/version-rules.html), which
filter out pre-releases, and the [FAQ](https://www.mojohaus.org/versions/versions-maven-plugin/faq.html).

## Contributing

Report problems in the [issue tracker](https://github.com/mojohaus/versions/issues) with the
plugin, Maven and JDK versions and a POM that reproduces them. For a larger change, open an
issue or write to the [mailing list](https://www.mojohaus.org/versions/mailing-lists.html) first.

Pull requests are squash-merged, and the release notes are built from their titles and labels,
so the title should describe the change for a user. [AGENTS.md](AGENTS.md) describes the modules,
the build gates and how the tests are written; it applies to human contributors as well.

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
