# Release Notes

## 2.5 (NOT YET RELEASED)

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
[issue-162]: https://github.com/jenkinsci/java-client-api/issues/162
[issue-166]: https://github.com/jenkinsci/java-client-api/issues/166
[issue-167]: https://github.com/jenkinsci/java-client-api/issues/167
[issue-168]: https://github.com/jenkinsci/java-client-api/issues/168
