title: Recording Changes
author: Mark Raynsford
date: 2020-06-27

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

# Recording Changes

Here's an example:

```sh
mvn versions:use-latest-releases -DchangeRecorderFormat=xml
```

Which writes a file to `target/versions-changes.xml` that looks something like:

```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<updates xmlns="http://www.mojohaus.org/versions-maven-plugin/schema/updates/2.0">
  <dependencyUpdate kind="dependency-update"
                    artifactId="dummy-api"
                    groupId="localhost"
                    newVersion="3.0"
                    oldVersion="1.1.1-2"/>
  <propertyUpdate property="revision"
                  newValue="3.0"
                  oldValue="3.1"/>
</updates>
```

The contents of this file records the fact that `localhost:dummy-api:1.1.1-2`
was upgraded to `3.0` and that the property `revision` changed its value from `3.0` to `3.1`.
