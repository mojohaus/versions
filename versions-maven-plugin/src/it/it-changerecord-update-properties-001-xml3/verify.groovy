import groovy.xml.XmlSlurper

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "update-properties"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.dependencyUpdate.any {node ->
    node.@kind == 'property-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-api'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.0'
}
assert changeRecorderLog.updates.propertyUpdate.any {node ->
    node.@property == 'api'
            && node.@oldValue == '1.0'
            && node.@newValue == '3.0'
}
