import groovy.xml.XmlSlurper
import groovy.json.JsonSlurper

output = new File(basedir, "output1.txt").text
assert output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+3\.0\b/

output = new File(basedir, "output2.txt").text
assert !(output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+3\.0\b/)
assert output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+2\.1\b/

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "display-dependency-updates"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.dependencyUpdate.any {node ->
    node.@kind == 'dependency-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-api'
            && node.@oldVersion == '1.1'
            && node.@newVersion == '3.0'
}

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "display-dependency-updates"
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.updateClass == 'dependency'
            && node.kind == 'dependency-update'
            && node.groupId == 'localhost'
            && node.artifactId == 'dummy-api'
            && node.oldVersion == '1.1'
            && node.newVersion == '2.1'
}

def csvChanges = new File(basedir, 'target/versions-changes.csv').text
assert csvChanges =~ "[^;]+;display-dependency-updates;[^;]+;dependency;dependency-update;localhost;dummy-api;;1\\.1;3\\.0;"
