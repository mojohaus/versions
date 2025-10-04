import groovy.json.JsonSlurper

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "update-parent"
def node = jsonChanges.updates[0].versionChanges[0]
assert node.kind == 'PARENT_UPDATE'
assert node.groupId == 'localhost'
assert node.artifactId == 'dummy-parent'
assert node.oldVersion == '1.0'
assert node.newVersion == '3.0'
