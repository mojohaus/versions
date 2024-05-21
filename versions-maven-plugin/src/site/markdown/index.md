title: Introduction
author: Stephen Connolly
date: 2009-04-21

<!---
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at
https://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

# Versions Maven Plugin

The Versions Plugin is used when you want to manage the versions of artifacts in a project's POM.

## Goals Overview

The Versions Plugin has the following goals.
* [versions:compare-dependencies](./compare-dependencies-mojo.html) compares the dependency versions of the current
project to the dependency management section of a remote project.
* [versions:display-dependency-updates](./display-dependency-updates-mojo.html) scans a project's dependencies and
produces a report of those dependencies which have newer versions available.
* [versions:display-plugin-updates](./display-plugin-updates-mojo.html) scans a project's plugins and produces a
report of those plugins which have newer versions available, taking care of Maven version prerequisites.
* [versions:display-property-updates](./display-property-updates-mojo.html) scans a project and produces a report of
those properties which are used to control artifact versions and which properties have newer versions available.
* [versions:update-parent](./update-parent-mojo.html) updates the parent section of a project so that it references the
newest available version.  For example, if you use a corporate root POM, this goal can be helpful if you need
to ensure you are using the latest version of the corporate root POM.
* [versions:update-properties](./update-properties-mojo.html) updates properties defined in a project so that they
correspond to the latest available version of specific dependencies.  This can be useful if a suite of dependencies
must all be locked to one version.
* [versions:update-property](./update-property-mojo.html) Sets a property to the latest version in a given range of
associated artifacts.
* [versions:update-child-modules](./update-child-modules-mojo.html) updates the parent section of the child modules
of a project so the version matches the version of the current project. For example, if you have an aggregator pom
that is also the parent for the projects that it aggregates and the children and parent versions get out of sync, this
mojo can help fix the versions of the child modules. (Note you may need to invoke Maven with the -N option in order to
run this goal if your project is broken so badly that it cannot build because of the version mis-match).
* [versions:lock-snapshots](./lock-snapshots-mojo.html) searches the pom for all -SNAPSHOT versions and replaces them
with the current timestamp version of that -SNAPSHOT, e.g. -20090327.172306-4
* [versions:unlock-snapshots](./unlock-snapshots-mojo.html) searches the pom for all timestamp locked snapshot versions
and replaces them with -SNAPSHOT.
* [versions:resolve-ranges](./resolve-ranges-mojo.html) finds dependencies using version ranges and resolves the range
to the specific version being used.
* [versions:set](./set-mojo.html) can be used to set the project version from the command line.
* [versions:set-property](./set-property-mojo.html) can be used to set one or multiple properties to a given version from the command line.
* [versions:use-releases](./use-releases-mojo.html) searches the pom for all -SNAPSHOT versions which have been
released and replaces them with the corresponding release version.
* [versions:use-next-releases](./use-next-releases-mojo.html) searches the pom for all non-SNAPSHOT versions which
have been a newer release and replaces them with the next release version.
* [versions:use-latest-releases](./use-latest-releases-mojo.html) searches the pom for all dependencies using *release* versions (i.e. not SNAPSHOT and not versions ending with a year-month-day suffix) and replaces them with the latest *release* version.
* [versions:use-next-snapshots](./use-next-snapshots-mojo.html) searches the pom for all non-SNAPSHOT versions which
have been a newer -SNAPSHOT version and replaces them with the next -SNAPSHOT version.
* [versions:use-latest-snapshots](./use-latest-snapshots-mojo.html) searches the pom for all non-SNAPSHOT versions
which have been a newer -SNAPSHOT version and replaces them with the latest -SNAPSHOT version.
* [versions:use-next-versions](./use-next-versions-mojo.html) searches the pom for all versions which
have been a newer version and replaces them with the next version.
* [versions:use-latest-versions](./use-latest-versions-mojo.html) searches the pom for all versions which
have been a newer version and replaces them with the latest version.
* [versions:use-dep-version](./use-dep-version-mojo.html) updates a dependency to a specific version.
* [versions:commit](./commit-mojo.html) removes the `pom.xml.versionsBackup` files. Forms one half of the
built-in "Poor Man's SCM".
* [versions:revert](./revert-mojo.html) restores the `pom.xml` files from the `pom.xml.versionsBackup` files.
Forms one half of the built-in "Poor Man's SCM".

## Reporting goals overview

The Versions Plugin has the following reporting goals.
* [versions:dependency-updates-report](./dependency-updates-report-mojo.html) produces a report of those
project dependencies which have newer versions available.
* [versions:dependency-updates-aggregate-report](./dependency-updates-aggregate-report-mojo.html) produces an aggregated
report(project + its submodules) of those dependencies which have newer versions available.
* [versions:plugin-updates-report](./plugin-updates-report-mojo.html) produces a report of those plugins which have
newer versions available.
* [versions:plugin-updates-aggregate-report](./plugin-updates-aggregate-report-mojo.html) produces an aggregated report
(project + its submodules) of those plugins which have newer versions available.
* [versions:property-updates-report](./property-updates-report-mojo.html) produces a report of
those properties which are used to control artifact versions and which properties have newer versions available.
* [versions:property-updates-aggregate-report](./property-updates-aggregate-report-mojo.html) produces an aggregated
report(project + its submodules) of those properties which are used to control artifact versions and
which properties have newer versions available.
* [versions:parent-updates-report](./parent-updates-report-mojo.html) produces a report on possible parent artifact
upgrades.

## Enforcer rules overview

The Versions Plugin currently provides one Maven Enforcer Plugin rule:
* [maxDependencyUpdates](../versions-enforcer/index.html) allows the user to specify a maximum number of updates which,
if exceeded, will trigger the enforcer plugin to fail.

## Usage

General instructions on how to use the Versions Plugin can be found on the [usage page](./usage.html). Some more
specific use cases are described in the examples given below.

In case you still have questions regarding the plugin's usage, please have a look at the [FAQ](./faq.html) and feel
free to contact the [user mailing list](./mail-lists.html). The posts to the mailing list are archived and could
already contain the answer to your question as part of an older thread. Hence, it is also worth browsing/searching
the [mail archive](./mail-lists.html).

If you feel like the plugin is missing a feature or has a defect, you can fill a feature request or bug report in our
[issue tracker](./issue-tracking.html). When creating a new issue, please provide a comprehensive description of your
concern. Especially for fixing bugs it is crucial that the developers can reproduce your problem. For this reason,
entire debug logs, POMs or most preferably little demo projects attached to the issue are very much appreciated.
Of course, patches are welcome, too. Contributors can check out the project from our
[source repository](./source-repository.html) and will find supplementary information in the
[guide to helping with Maven](https://maven.apache.org/guides/development/guide-helping.html).

## Examples

To provide you with better understanding of some usages of the Plugin Name,
you can take a look into the following examples:
* [Advancing dependency versions](./examples/advancing-dependency-versions.html)
* [Compare project dependencies to a remote project](./examples/compare-dependencies.html)
* [Checking for new dependency updates](./examples/display-dependency-updates.html)
* [Checking for new plugin updates](./examples/display-plugin-updates.html)
* [Checking for new property-linked updates](./examples/display-property-updates.html)
* [Updating the Parent version](./examples/update-parent.html)
* [Updating a version specified in a property](./examples/update-properties.html)
* [Fixing a multi-module build](./examples/update-child-modules.html)
* [Resolve version ranges](./examples/resolve-ranges.html)
* [Locking snapshot dependencies](./examples/lock-snapshots.html)
* [Unlocking snapshot dependencies](./examples/unlock-snapshots.html)
* [Replacing -SNAPSHOT versions with their corresponding releases](./examples/use-releases.html)
* [Changing the project version](./examples/set.html)
* [Recording version changes](./examples/recording-changes.html)
