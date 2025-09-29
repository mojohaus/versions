import groovy.xml.XmlSlurper

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "update-parent"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.dependencyUpdate.any {node ->
    node.@kind == 'parent-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-parent'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.0'
}
