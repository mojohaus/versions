title: Using use-dynamic-version-from-scm goal
author: Jimisola Laursen
date: 2024-06-22

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

The `use-dynamic-version-from-scm` goal will use scm/vcs (currently, only git) tags to determine the version for the current build and set that in a property (default: `revision`). It is intended to be used with [Maven CI Friendly Versions](https://maven.apache.org/maven-ci-friendly.html). The goal follows [Versions Maven Plugin - Version number rules](https://www.mojohaus.org/versions/versions-maven-plugin/version-rules.html).

* The version tag can be with or without "v" prefix, i.e. "v1.2.3" or "1.2.3".
* The `-SNAPSHOT` qualifier suffix is optional (appended per default).
* If the parameter `useVersion` is set then that version will be used irrespective of commits/tags (is typically used for testing).
* The goal will determine the version for the current build as follows:
  * latest commit has valid version tag: use the highest version tag for that commit (e.g. 1.2.3 -> 1.2.3)
  * latest version has _no_ valid version tag: use the highest version tag for the most recent non-latest commit, increase incremental (patch) version with 1 and use the number of commits to that commit (commit count) as build number and `-SNAPSHOT qualifer appended (e.g. 1.2.3 -> 1.2.4-12-SNAPSHOT)
  * no commit with valid version tag: use 0.0.1 (configurable) and use the number of commits to that commit (commit count) as build number and `-SNAPSHOT qualifer appended (e.g. 1.2.3 -> 1.2.4-12-SNAPSHOT) (e.g. 0.0.1-10-SNAPSHOT)
  * repository has no commits: build fails with exception `Caused by: org.apache.maven.plugin.MojoExecutionException: SCM repo has no head/commits.`

