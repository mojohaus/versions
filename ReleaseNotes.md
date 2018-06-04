# Release Notes

## 2.6

 * [Pull Request #252][pull-252]

   Thanks to Edward Maxwell-Lyte <2248005+edwardmlyte@users.noreply.github.com>

   Minor spelling corrections

 * [Fixed Issue 157][issue-157]

    Document the end of versioning limitations in Maven 3.x
    There may remain good reasons for defining custom versioning rules to
    let versions-maven-plugin apply, but at least not the old Maven 2.x
    limitation

 * [Fixed Issue 256][issue-256]

    if initial and new version are equals, just display initial
    this means this is a plugin version that requires a Maven version that
    is not compatible with project minimum version, not really a proposed
    upgrade

 * [Fixed Issue 237][issue-237]

   Thanks to Julian Di Leonardo <DiJu519@users.noreply.github.com>

   Adding parent processing to UseLatestVersion/UseLatestSnapshot/UseLatestRelease

 * [Fixed Issue 190][issue-190]

   Thanks to Julian Di Leonardo <DiJu519@users.noreply.github.com>

   Fixing issue in update-child-modules, where root module's version was
   being used in all downstream children even when a child's parent was
   different.
   
 * [Fixed Issue 219][issue-219]
   
   Added threadSafe=true to goals to prevent
   warning in Maven builds if you start Maven
   via: mvn -T ..

 * [Fixed Issue 215][issue-215]

## 2.5


 * [Fixed Issue 202][issue-202]

   -DprocessParent=true is ignored by user-reactor mojo.

 * [Fixed Issue 182][issue-182]

   Add goal for updating the SCM tag in the POM.

 * [Fixed Issue 197][issue-197]

   Java 1.7 as prerequisite.

 * [Fixed Issue 198][issue-198]

   Update version of modules which are not children but part of reactor.

 * [Fixed Issue 185][issue-185]

   Unable to set dependencyReportFormat as parameter anymore.
   Thanks to Ilja Dubinin.

 * [Fixed Issue 187][issue-187]

   create target directory when run dependency update report.
   Thanks to Ilja Dubinin.

 * [Pull Request #189][pull-189]

   Fixed inccorect links. Thanks to Anton Johansson.

 * [Fixed Issue 177][issue-177]
   
   A required class was missing while executing on dependency-updates-report
   
 * [Fixed Issue 129][issue-129]
   
   dependency-update-report now is able to disable the processing 
   of the transitive dependencies in the dependencyManagement part.
   This can be enabled (default)/disabled by setting
   processDependencyManagementTransitive accordingly.

 * [Fixed Issue 166][issue-166]
   
   Upgraded modello-maven-plugin to 1.9.1

 * [Fixed Issue 167][issue-167]
  
   Upgraded versions-maven-plugin from 2.1 to 2.4

 * [Fixed Issue 168][issue-168]
 
   Added allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates
   to resolve-ranges and display-property-updates goal.

 * [Fixed Issue 37][issue-37]
   
   Enhanced documentation for aggegator modules.
     
 * [Fixed Issue 46][issue-46]
 
   Added processDependencyManagement option to control the reporting
   of dependencyManagement.
   Thanks to Bruce Brouwer.

 * [Fixed Issue 94][issue-94]
 
   introduced allowMajorUpdates, allowMinorUpdates,
   allowIncrementalUpdates.
   introduced allowAnyUpdates for compatibility which
   will be removed in 3.0.0 of the plugin.

 * [Fixed Issue 34][issue-34]
   
   Added implementation to report the plugin dependencies as well.
   Added IT's for reporting updates about dependencies in plugins,
   plugins defined in pluginManagement.

* [Fixed Issue 162][issue-162]

  Upgraded cobertura-maven-plugin used in report section
  to 2.7.
   


[issue-34]: https://github.com/mojohaus/versions-maven-plugin/issues/34
[issue-37]: https://github.com/mojohaus/versions-maven-plugin/issues/37
[issue-46]: https://github.com/mojohaus/versions-maven-plugin/issues/46
[issue-94]: https://github.com/mojohaus/versions-maven-plugin/issues/94
[issue-129]: https://github.com/mojohaus/versions-maven-plugin/issues/129
[issue-157]: https://github.com/mojohaus/versions-maven-plugin/issues/157
[issue-162]: https://github.com/mojohaus/versions-maven-plugin/issues/162
[issue-166]: https://github.com/mojohaus/versions-maven-plugin/issues/166
[issue-167]: https://github.com/mojohaus/versions-maven-plugin/issues/167
[issue-168]: https://github.com/mojohaus/versions-maven-plugin/issues/168
[issue-177]: https://github.com/mojohaus/versions-maven-plugin/issues/177
[issue-182]: https://github.com/mojohaus/versions-maven-plugin/issues/182
[issue-185]: https://github.com/mojohaus/versions-maven-plugin/issues/185
[issue-187]: https://github.com/mojohaus/versions-maven-plugin/issues/187
[issue-190]: https://github.com/mojohaus/versions-maven-plugin/issues/190
[issue-202]: https://github.com/mojohaus/versions-maven-plugin/issues/202
[issue-215]: https://github.com/mojohaus/versions-maven-plugin/issues/215
[issue-219]: https://github.com/mojohaus/versions-maven-plugin/issues/219
[issue-197]: https://github.com/mojohaus/versions-maven-plugin/issues/197
[issue-198]: https://github.com/mojohaus/versions-maven-plugin/issues/198
[issue-237]: https://github.com/mojohaus/versions-maven-plugin/issues/237
[issue-256]: https://github.com/mojohaus/versions-maven-plugin/issues/256

[pull-189]: https://github.com/mojohaus/versions-maven-plugin/pull/189
[pull-252]: https://github.com/mojohaus/versions-maven-plugin/pull/252
