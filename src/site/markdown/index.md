title: Versions

<!---
Copyright MojoHaus and Contributors

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Versions

The Versions Maven Plugin shows which dependencies, plugins and properties in your POM have newer versions, and
updates them for you. It also sets the version of a multi-module project in one step.

## Quick start

Run these from the directory that holds your `pom.xml`. No change to the POM is needed.

List dependencies with newer versions:

```
mvn versions:display-dependency-updates
```

List plugins with newer versions, taking each plugin's Maven prerequisite into account:

```
mvn versions:display-plugin-updates
```

Update the version properties of your POM to the newest releases:

```
mvn versions:update-properties
```

Set the version of a project and all its modules:

```
mvn versions:set -DnewVersion=2.0.0
```

## Documentation

* [Versions Maven Plugin](./versions-maven-plugin/index.html): what each goal does.
* [Goals](./versions-maven-plugin/plugin-info.html): the parameters of every goal.
* [Usage](./versions-maven-plugin/usage.html): running the goals and configuring the plugin.
* [Version number rules](./versions-maven-plugin/version-rules.html): how versions are compared and how to ignore
  some of them.
* [FAQ](./versions-maven-plugin/faq.html)
* [Versions Enforcer](./versions-enforcer/index.html): an Enforcer rule that fails the build when too many dependencies
  are out of date.

## For contributors

The plugin is built from these modules:

* [Versions API](./versions-api/index.html): the extension API for the plugin.
* [Versions Common](./versions-common/index.html): code shared by the plugin and the Enforcer rule.
* [Versions Model](./versions-model/index.html) and [Versions Model Report](./versions-model-report/index.html): the
  Modello models of the rule sets and reports.
* [Versions Test](./versions-test/index.html): test utilities.

The source is on [GitHub](https://github.com/mojohaus/versions).
