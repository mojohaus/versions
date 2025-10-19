title: Checking for new dependency updates
author: Stephen Connolly
date: 2008-09-02

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

# Checking for new dependency updates

The `display-dependency-updates` goal will check all the dependencies used in your project and display a list
of those dependencies with newer versions available.

Here are some examples of what this looks like:

```sh
svn checkout http://svn.codehaus.org/mojo/trunk/mojo/build-helper-maven-plugin build-helper-maven-plugin
cd build-helper-maven-plugin
mvn versions:display-dependency-updates
```

Which produces the following output:

```sh
[INFO] ------------------------------------------------------------------------
[INFO] Building Build Helper Maven Plugin
[INFO]    task-segment: [versions:display-dependency-updates]
[INFO] ------------------------------------------------------------------------
[INFO] [versions:display-dependency-updates]
[INFO]
[INFO] The following dependency updates are available:
[INFO]   org.apache.maven:maven-artifact ........................ 2.0 -> 2.0.9
[INFO]   org.apache.maven:maven-plugin-api ...................... 2.0 -> 2.0.9
[INFO]   org.apache.maven:maven-project ....................... 2.0.2 -> 2.0.9
[INFO]   org.codehaus.plexus:plexus-utils ....................... 1.1 -> 1.5.6
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESSFUL
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 17 seconds
[INFO] Finished at: Fri Aug 15 10:46:03 IST 2008
[INFO] Final Memory: 10M/167M
[INFO] ------------------------------------------------------------------------
```

# Ignore a specific version suffix in version updates

Let's suppose you wanted `org.apache.maven.doxia:doxia-core:` not to be updated to `2.0.0-M6` or `org.apache.maven:maven-core` to `4.0.0-alpha-5'  or anything with a `-M` or `-alpha' in it. Upon first sight of the `dependencyExcludes` option, one might consider to use it to filter out anything with `2.*-M.*` or `*-alpha.*`.

Well, that would be wrong. `dependencyIncludes` and `dependencyExcludes` work only on *input* dependencies, that is, dependency versions that are already being used by your project. This means that it is likely that you will still see the dreaded `2.0.0-M6`-like version in the updates.

You can either use `ruleSet` or `ignoredVersions`. The former allows for a greater control where you can specify ignored version patterns per dependency whereas the latter is intended to be used from command line and only offers simple version filters.

So, let's say we want to display dependency updates of this very plugin and while doing so, ignore all updates with an `-M` or `-alpha` at the end of the version string, simply use:

```shell
mvn org.codehaus.mojo:versions-maven-plugin:display-dependency-updates "-Dmaven.version.ignore=.*-M.*,.*-alpha.*"
```

or:

```xml
<configuration>
    ...
    <ignoredVersions>.*-M.*,.*-alpha.*</ignoredVersions>
    ...
</configuration>
```

Alternatively, with greater readability, you can write:

```xml
<configuration>
    ...
    <ignoredVersions>
        <ignoredVersion>.*-M.*</ignoredVersion>
        <ignoredVersion>.*-alpha.*</ignoredVersion>
    </ignoredVersions>
    ...
</configuration>
```

in your project config. That will result in the following output. Instead of:

```shell
[INFO] --- versions:2.15.0:display-dependency-updates (default-cli) @ versions-maven-plugin ---
[INFO] The following dependencies in Dependency Management have newer versions:
[INFO]   dom4j:dom4j ................................. 1.6.1 -> 20040902.021138
[INFO]   org.apache.maven:maven-artifact ............... 3.2.5 -> 4.0.0-alpha-5
[INFO]   org.apache.maven:maven-compat ................. 3.2.5 -> 4.0.0-alpha-5
[INFO]   org.apache.maven:maven-core ................... 3.2.5 -> 4.0.0-alpha-5
[INFO]   org.apache.maven:maven-model .................. 3.2.5 -> 4.0.0-alpha-5
[INFO]   org.apache.maven:maven-plugin-api ............. 3.2.5 -> 4.0.0-alpha-5
[INFO]   org.apache.maven:maven-settings ............... 3.2.5 -> 4.0.0-alpha-5
[INFO]   org.apache.maven.enforcer:enforcer-api ................ 3.2.1 -> 3.3.0
[INFO]   org.apache.maven.plugin-testing:maven-plugin-testing-harness ...
[INFO]                                                   3.3.0 -> 4.0.0-alpha-1
[INFO]   org.apache.maven.plugin-tools:maven-plugin-annotations ...
[INFO]                                                           3.8.1 -> 3.8.2
[INFO]   org.mockito:mockito-inline ........................... 4.11.0 -> 5.2.0
[INFO]   org.slf4j:slf4j-simple ............................... 1.7.36 -> 2.0.7
```

you will only see:

```shell
[INFO] --- versions:2.15.0:display-dependency-updates (default-cli) @ versions-maven-plugin ---
[INFO] The following dependencies in Dependency Management have newer versions:
[INFO]   dom4j:dom4j ................................. 1.6.1 -> 20040902.021138
[INFO]   org.apache.maven:maven-artifact ....................... 3.2.5 -> 3.9.2
[INFO]   org.apache.maven:maven-compat ......................... 3.2.5 -> 3.9.2
[INFO]   org.apache.maven:maven-core ........................... 3.2.5 -> 3.9.2
[INFO]   org.apache.maven:maven-model .......................... 3.2.5 -> 3.9.2
[INFO]   org.apache.maven:maven-plugin-api ..................... 3.2.5 -> 3.9.2
[INFO]   org.apache.maven:maven-settings ....................... 3.2.5 -> 3.9.2
[INFO]   org.apache.maven.enforcer:enforcer-api ................ 3.2.1 -> 3.3.0
[INFO]   org.apache.maven.plugin-tools:maven-plugin-annotations ...
[INFO]                                                           3.8.1 -> 3.8.2
[INFO]   org.mockito:mockito-inline ........................... 4.11.0 -> 5.2.0
[INFO]   org.slf4j:slf4j-simple ............................... 1.7.36 -> 2.0.7
```

