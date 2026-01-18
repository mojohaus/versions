import groovy.xml.XmlSlurper
import groovy.json.JsonSlurper

def output = new File(basedir, "output.txt").text
assert output =~ /\Qlocalhost:dummy-parent2\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E\b/

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "display-parent-updates"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.dependencyUpdate.any {node ->
    node.@kind == 'parent-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-parent2'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.1'
}

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "display-parent-updates"
assert jsonChanges.updates[0].versionChanges.any { node ->
    node.updateClass == 'dependency'
    && node.kind == 'parent-update'
    && node.groupId == 'localhost'
    && node.artifactId == 'dummy-parent2'
    && node.oldVersion == '1.0'
    && node.newVersion == '3.1'
}

def csvChanges = new File(basedir, 'target/versions-changes.csv').text
assert csvChanges =~ "[^;]+;display-parent-updates;[^;]+;dependency;parent-update;localhost;dummy-parent2;;1.0;3.1;"
