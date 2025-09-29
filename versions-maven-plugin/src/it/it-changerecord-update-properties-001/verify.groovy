import groovy.xml.XmlSlurper

def changes = new XmlSlurper().parse(new File(basedir, 'target/versions-changes.xml'))
assert changes.dependencyUpdate.any {node ->
    node.@kind == 'property-update'
            && node.@groupId == 'localhost'
            && node.@artifactId == 'dummy-api'
            && node.@oldVersion == '1.0'
            && node.@newVersion == '3.0'
}
assert changes.propertyUpdate.any {node ->
    node.@property == 'api'
            && node.@oldValue == '1.0'
            && node.@newValue == '3.0'
}
