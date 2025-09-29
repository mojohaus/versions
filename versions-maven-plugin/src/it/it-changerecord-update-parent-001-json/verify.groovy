import groovy.json.JsonSlurper

def jsonChanges = new JsonSlurper().parse(new File(basedir, 'target/versions-changes.json'))
assert jsonChanges.updates[0].goal == "update-parent"
assert jsonChanges.updates[0].versionChanges.any {node ->
    node.updateClass == 'dependency'
            && node.kind == 'parent-update'
            && node.groupId == 'localhost'
            && node.artifactId == 'dummy-parent'
            && node.oldVersion == '1.0'
            && node.newVersion == '3.0'
}
