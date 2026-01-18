import groovy.json.JsonSlurper
import groovy.xml.XmlSlurper

def output1 = new File(basedir, "output1.txt").text
assert output1 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/
assert output1 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/

def output2 = new File(basedir, "output2.txt")
assert !output2.exists()

def output3 = new File(basedir, "output3.txt").text
assert output3 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/
assert !(output3 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/)

def changeRecorderLog = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changeRecorderLog.updates.@goal == "display-extension-updates"
assert !changeRecorderLog.updates.@date.isEmpty()
assert changeRecorderLog.updates.extensionUpdate.any {node ->
    node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-maven-plugin'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.1'
}
assert changeRecorderLog.updates.extensionUpdate.any {node ->
    node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-api'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.0'
}

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "display-extension-updates"
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.updateClass == 'extension'
            && node.groupId == 'localhost'
            && node.artifactId == 'dummy-maven-plugin'
            && node.oldVersion == '1.0'
            && node.newVersion == '3.1'
}

def csvChanges = new File(basedir, 'target/versions-changes.csv').text
assert csvChanges =~ "[^;]+;display-extension-updates;[^;]+;extension;;localhost;dummy-maven-plugin;;1.0;3.1;"
assert csvChanges =~ "[^;]+;display-extension-updates;[^;]+;extension;;localhost;dummy-api;;1.0;3.0;"
