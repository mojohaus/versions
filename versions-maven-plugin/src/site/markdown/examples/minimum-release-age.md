title: Holding back recent releases

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

# Holding back recent releases

A release that was published a few hours ago may be faulty, or may have been published by someone who should not
have been able to. The `minimumReleaseAge` parameter keeps such a release out of the updates: a version newer than
the one in use is offered only once it has been published for the given number of days. Renovate calls the same
idea `minimumReleaseAge`, and Dependabot calls it `cooldown`.

The parameter applies to every goal and report that looks for newer versions, for example:

```sh
mvn versions:display-dependency-updates -Dversions.minimumReleaseAge=7
mvn versions:use-latest-releases -Dversions.minimumReleaseAge=7
```

The default is `0`, which switches the filter off. To use it on every run, set it in the plugin configuration:

```xml
<plugin>
  <groupId>org.codehaus.mojo</groupId>
  <artifactId>versions-maven-plugin</artifactId>
  <configuration>
    <minimumReleaseAge>7</minimumReleaseAge>
  </configuration>
</plugin>
```

A version that is older than or equal to the current one is not affected, and neither is a snapshot: whether
snapshots are offered is up to `allowSnapshots`.

## Trusted artifacts

`minimumReleaseAgeExcludes` lists `groupId:artifactId` patterns of artifacts that are exempt, for artifacts whose
sources you trust. A `*` matches any run of characters, and the `artifactId` may be left out to match the whole
group. On the command line, separate the patterns with commas:

```sh
mvn versions:display-dependency-updates -Dversions.minimumReleaseAge=7 \
    -Dversions.minimumReleaseAgeExcludes='com.example:*,org.codehaus.mojo'
```

In the POM, write one element per pattern:

```xml
<configuration>
  <minimumReleaseAge>7</minimumReleaseAge>
  <minimumReleaseAgeExcludes>
    <minimumReleaseAgeExclude>com.example:*</minimumReleaseAgeExclude>
    <minimumReleaseAgeExclude>org.codehaus.mojo</minimumReleaseAgeExclude>
  </minimumReleaseAgeExcludes>
</configuration>
```

## Where the publication date comes from

The date is the modification time of the version's POM, as the Maven Resolver delivers it from the repository, which
stamps a downloaded file with the `Last-Modified` header of the server. The lookup goes through Maven's own
transport, so the proxy, mirror and server settings of your `settings.xml` apply.

A POM that was not downloaded from an HTTP repository, for example one that `mvn install` copied into the local
repository, carries the time it was written and counts as published then. If the plugin cannot determine the date
of a version, it offers that version and logs a warning, so the filter is never switched off unnoticed.
