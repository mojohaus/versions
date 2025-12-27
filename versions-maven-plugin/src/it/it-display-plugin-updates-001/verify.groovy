import groovy.json.JsonSlurper
import groovy.xml.XmlSlurper

output = new File(basedir, "output1.txt").text
assert output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+3\.0\b/

output = new File(basedir, "output2.txt").text
assert ! ( output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+3\.0\b/ )
assert output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+2\.1\b/

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "display-plugin-updates"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.pluginUpdate.any {node ->
    node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-maven-plugin'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.0'
}
assert changeRecorderLog.updates.pluginUpdate.any {node ->
    node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-maven-plugin'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.1'
            && node.@minimumMavenVersion == '3.8.4'
}

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "display-plugin-updates"
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.updateClass == 'plugin'
            && node.groupId == 'localhost'
            && node.artifactId == 'dummy-maven-plugin'
            && node.oldVersion == '1.0'
            && node.newVersion == '2.1'
}
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.updateClass == 'plugin'
            && node.groupId == 'localhost'
            && node.artifactId == 'dummy-maven-plugin'
            && node.oldVersion == '1.0'
            && node.newVersion == '3.1'
            && node.minimumMavenVersion == '3.8.4'
}

def csvChanges = new File(basedir, 'target/versions-changes.csv').text
assert csvChanges =~ "[^;]+;display-plugin-updates;[^;]+;plugin;;localhost;dummy-maven-plugin;;1.0;3.0;"
assert csvChanges =~ "[^;]+;display-plugin-updates;[^;]+;plugin;;localhost;dummy-maven-plugin;;1.0;3.1;3.8.4"
