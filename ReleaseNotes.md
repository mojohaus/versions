# Release Notes

## 2.5 (NOT YET RELEASED)


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
   

[issue-34]: https://github.com/jenkinsci/java-client-api/issues/34
[issue-37]: https://github.com/jenkinsci/java-client-api/issues/37
[issue-46]: https://github.com/jenkinsci/java-client-api/issues/46
[issue-94]: https://github.com/jenkinsci/java-client-api/issues/94
[issue-129]: https://github.com/jenkinsci/java-client-api/issues/129
[issue-162]: https://github.com/jenkinsci/java-client-api/issues/162
[issue-166]: https://github.com/jenkinsci/java-client-api/issues/166
[issue-167]: https://github.com/jenkinsci/java-client-api/issues/167
[issue-168]: https://github.com/jenkinsci/java-client-api/issues/168
[issue-177]: https://github.com/jenkinsci/java-client-api/issues/177
[issue-182]: https://github.com/mojohaus/versions-maven-plugin/issues/182
[issue-185]: https://github.com/mojohaus/versions-maven-plugin/issues/185
[issue-187]: https://github.com/mojohaus/versions-maven-plugin/issues/187
[pull-189]: https://github.com/mojohaus/versions-maven-plugin/pull/189
[issue-197]: https://github.com/mojohaus/versions-maven-plugin/issues/197
[issue-198]: https://github.com/mojohaus/versions-maven-plugin/issues/198

