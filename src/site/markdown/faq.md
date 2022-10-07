title: FAQ
autor: Stephen Connolly

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

# Frequently Asked Questions

## General

### What does the Versions Maven Plugin do?

The Versions Maven Plugin provides a means to update version information in a Maven project

### Why is this plugin reporting version 1.0.0.9 of foo:bar as the latest version when I can see version 1.0.0.23?

The current implementation of
[DefaultArtifactVersion](https://maven.apache.org/ref/current/maven-artifact/xref/org/apache/maven/artifact/versioning/DefaultArtifactVersion.html)
in the core of Maven expects that version numbers will have a very specific format:

` <MajorVersion [> . <MinorVersion [> . <IncrementalVersion ] ] [> - <BuildNumber | Qualifier ]> `

Where *MajorVersion*, *MinorVersion*, *IncrementalVersion* and *BuildNumber* are all numeric and *Qualifier*
is a string. If your version number does not match this format, then the entire version number is treated as being 
the *Qualifier*.

Version numbers in maven are compared using the individual components, so *MajorVersion*, *MinorVersion*,
*IncrementalVersion*, and *BuildNumber* are all compared as `Integer`s while *Qualifier* is compared as a `String`.

From the above you may have guessed neither `1.0.0.9` nor `1.0.0.23` match the exact format that Maven expects,
and as a result they are mapped as being just a *Qualifier*. String comparison will sort `1.0.0.9` > `1.0.0.23`.

If you need version numbers to be sorted "correctly" you will need to define some
[version number comparison rules](./version-rules.html).

### Why is foo:bar version x.y.z not being detected?

In order to determine what versions of an artifact are present, Maven relies on the presence of
meta-data files in the repository.  If the meta-data files are missing or contain invalid information
then Maven will not know about versions that are available in your repositories.

Here are some common reasons why your metadata can be invalid:

- You are using a local repository as a remote repository. Stop, don't do this. The local repository
uses a separate set of metadata files from those used by a remote repository. There are tools available
to convert a local repository into a remote repository, but you're really better off using a repository
manager.
- Artifacts have been deployed by non-maven metadata aware tools.
- Tools that claim to be maven meta-data aware have clobbered the metadata files.

In most cases, using a repository manager will solve these issues as the repository managers usually
rebuild the metadata files based on the artifacts that are present.
