# MojoHaus Versions Maven Plugin

This is the [versions-maven-plugin](http://www.mojohaus.org/versions-maven-plugin/).

[![Apache License, Version 2.0, January 2004](https://img.shields.io/github/license/mojohaus/versions-maven-plugin.svg?label=License)](http://www.apache.org/licenses/)
[![Maven Central](https://img.shields.io/maven-central/v/org.codehaus.mojo/versions-maven-plugin.svg?label=Maven%20Central)](https://search.maven.org/artifact/org.codehaus.mojo/versions-maven-plugin)
[![Build Status](https://github.com/mojohaus/versions-maven-plugin/workflows/GitHub%20CI/badge.svg?branch=master)](https://github.com/mojohaus/versions-maven-plugin/actions/workflows/maven.yml?query=branch%3Amaster)

## Maintained versions

Versions Maven Plugin requires Maven 3.6.3+ and JDK 1.8+.

However, we maintain the latest plugin version alongside the latest Maven release.

We execute tests against different operating systems and JDKs
by [GitHub Actions](https://github.com/mojohaus/versions-maven-plugin/actions/workflows/maven.yml?query=branch%3Amaster)

## Contributing

### Creating Issues

If you find a problem, please first search the current open and closed issues and pull requests.
Someone may have already reported a similar issue.

You can also check the current [milestone](https://github.com/mojohaus/versions-maven-plugin/milestones)
to see what will be in the next release.

Only if you cannot find a similar issue, please create a new one in the
[ticket system](https://github.com/mojohaus/versions-maven-plugin/issues)
and describe what is going wrong or what you expect to happen.

If you have a full working example or a log file, this is also helpful.

Describe only a single issue per ticket; do not mix several issues into one.

Please always check your issue with the latest Plugin and the latest Maven version.

### Creating a Pull Request

Before working on a more complicated change or new feature,
it is good practice to create an issue in
the [ticket system](https://github.com/mojohaus/versions-maven-plugin/issues)
or send an email to [development list](https://www.mojohaus.org/versions-maven-plugin/mailing-lists.html)
and describe what the problem is or what kind of feature you would like to add.
Wait a few days for feedback from other contributors.
Afterwards you can create an appropriate pull request.

To have a pull request integrated, please squash your commits into a single commit
that references the issue in the commit message, for example:

```text
Fixed #Issue - change subject

a description
```

Note that the commit subject will be used in release notes and preserved in Git history,
so it should be sufficiently descriptive.

This simplifies merging and automatically closes the related issue. It also makes life
as a maintainer a little easier.

A pull request must address only a single ticket and should never create, add, or fix
several issues at once, as otherwise the history is hard to read and maintenance of
issues and pull requests becomes very difficult.

## Releasing

* Make sure `gpg-agent` is running.
* Execute `mvn -B release:prepare release:perform`

For publishing the site do the following:

```shell
cd target/checkout
mvn site
mvn scm-publish:publish-scm
```

For a multi-module site, two executions are needed.
