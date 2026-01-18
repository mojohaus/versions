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

Out of the box, the following recorders are provided:

| Name |                                                                                                                             Description                                                                                                                             |
|------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| none | An "empty" recorder that will not output any changes                                                                                                                                                                                                                |
| xml  | A recorder writing to an XML file conforming to the legacy http://www.mojohaus.org/versions-maven-plugin/schema/updates/2.0 namespace, or to the http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0 namespace if`legacy=false` is provided as options |
| json | A recorder writing to a Json file                                                                                                                                                                                                                                   |
| csv  | A recorder writing to a CSV file                                                                                                                                                                                                                                    |

## Legacy XML format

To generate a simple XML logs compatible with the legacy schema, use the following example:
Here's an example:

```shell
mvn versions:use-latest-releases -DchangeRecorderFormat=xml
```

By default, the report will be written to `target/versions-changes.xml`.

* If you'd rather that the file was written elsewhere, just use the additional `-DchangeRecorderOutputFileName` option, providing
  the filename (without its path), like so:

  ```shell
  mvn versions:use-latest-releases -DchangeRecorderFormat=xml -DchangeRecorderOutputFileName=changes.xml
  ```

The result should be:

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

## New XML format

The _legacy_ XML renderer does not allow appending to the change record once it's been written, so multiple executions of the plugin
will each overwrite the outcome of the previous execution. That's why there is now a new file format available.

At the time being, the new file format can be activated by providing a change recorder option:

```shell
mvn versions:use-latest-releases -DchangeRecorderFormat=xml -DchangeRecorderOptions=legacy=false
```

This will generate the following file:

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<changeRecorderLog xmlns="http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0">
  <updates goal="use-latest-releases" date="2025-09-28T12:59:41.458+02:00">
    <dependencyUpdate kind="dependency-update" groupId="localhost" artifactId="dummy-api" oldVersion="1.1.1-2" newVersion="3.0"/>
    <propertyUpdate property="revision" oldValue="1.1.1-2" newValue="3.0"/>
  </updates>
</changeRecorderLog>
```

As you can see, the new format features an aggregate type above the `updates` element; also, the `updates` element
now includes attributes providing the Maven goal as well as the date and time of the execution.

## JSON

To generate a report in Json, use the following command:

```shell
mvn versions:use-latest-releases -DchangeRecorderFormat=json
```

This will generate the following file:

Because the version changes array can contain different classes of updates, it features an additional discriminator
called `updateClass`; valid values are: `dependency`, `plugin`, `property`, `extension`.

```json
{
  "updates": [
    {
      "versionChanges": [
        {
          "updateClass": "dependency",
          "kind": "dependency-update",
          "groupId": "localhost",
          "artifactId": "dummy-api",
          "oldVersion": "1.1.1-2",
          "newVersion": "3.0"
        },
        {
          "updateClass": "property",
          "property": "revision",
          "oldValue": "1.1.1-2",
          "newValue": "3.0"
        }
      ],
      "goal": "use-latest-releases",
      "date": "2025-10-03T07:05:38.437013556+02:00"
    }
  ]
}
```

## CSV

To generate a report in a semicolon-separated CSV file, use the following command:

```shell
mvn versions:use-latest-releases -DchangeRecorderFormat=csv
```

Due to the fact that every row needs to contain information modelling different classes of changes, the row definition
differs from the XSD model.

Each row of the CSV document will consist of the following columns:

|   Column name   |                                                                                                                                Description                                                                                                                                |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| execution       | Randomly generated discriminator of the Maven execution (UUID); rows bearing the same UUID will belong to the same Maven execution                                                                                                                                        |
| goal            | Indicates the Maven goal that produced the report lines with the UUID above                                                                                                                                                                                               |
| updateClass     | Indicates the overarching class of the update: one of:`dependency`, `plugin`, `property`, `extension`                                                                                                                                                                     |
| kind            | Only defined for depenendency class updates; indicates the section where the dependency updates took place. One of:`project-update`, `dependency-update`, `dependency-management-update`, `parent-update`, `plugin-update`, `plugin-management-update`, `property-update` |
| groupId         | Contains the groupId of the artifact under change; unfilled for properties                                                                                                                                                                                                |
| artifactId      | Contains the artifactId of the artifact under change; unfilled for properties                                                                                                                                                                                             |
| property        | Contains the name of the property; only applicable for property updates                                                                                                                                                                                                   |
| oldValue        | Contains the old version or old property value                                                                                                                                                                                                                            |
| newValue        | Contains the new version or new property value                                                                                                                                                                                                                            |
| minMavenVersion | Contains the minimum Maven version applicable for the update; only applicable for plugin updates                                                                                                                                                                          |

And the result will be:

```csv
execution;goal;date;updateClass;kind;groupId;artifactId;property;oldValue;newValue;minMavenVersion
c0cbf50d-047a-483a-8046-6e5631f94365;use-latest-releases;2025-10-04T11:10:51.765+02:00;dependency;dependency-update;localhost;dummy-api;;1.1.1-2;3.0;
c0cbf50d-047a-483a-8046-6e5631f94365;use-latest-releases;2025-10-04T11:10:51.765+02:00;property;;;;revision;1.1.1-2;3.0;
```

