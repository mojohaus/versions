import groovy.json.JsonSlurper
import groovy.xml.XmlSlurper

output = new File(basedir, "output1.txt").text
assert output =~ /\Q\u0024{api}\E\s*\.*\s*1\.0\s+->\s+2\.0\b/

output = new File(basedir, "output2.txt").text
assert ! ( output =~ /\Q\u0024{api}\E\s*\.*\s*1\.0\s+->\s+2\.0\b/ )

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "display-property-updates"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.propertyUpdate.any {node ->
    node.@property == 'api'
            && node.@oldValue == '1.0'
            && node.@newValue == '2.0'
}
assert changeRecorderLog.updates.dependencyUpdate.any {node ->
    node.@kind == 'property-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-api'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '2.0'
}

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "display-property-updates"
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.property == 'api'
    node.oldValue == '1.0'
    node.newValue == '2.0'
}
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.kind == 'property-update'
    node.groupId == 'localhost'
    node.artifactId == 'dummy-api'
    node.oldVersion == '1.0'
    node.newVersion == '2.0'
}

def csvChanges = new File(basedir, 'target/versions-changes.csv').text
assert csvChanges =~ "[^;]+;display-property-updates;[^;]+;property;;;;api;1.0;2.0;"
assert csvChanges =~ "[^;]+;display-property-updates;[^;]+;dependency;property-update;localhost;dummy-api;;1.0;2.0;"
