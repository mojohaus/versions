import groovy.xml.XmlSlurper

def xmlChanges = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert xmlChanges.dependencyUpdate.any {node ->
    node.@kind == 'parent-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-parent'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.0'
}
