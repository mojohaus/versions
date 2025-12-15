import groovy.xml.XmlSlurper

def changes = new XmlSlurper().parse( new File( basedir, 'target/versions-changes.xml' ) )
assert (changes.dependencyUpdate.findAll { node -> node.@kind == 'property-update'
        && node.@groupId == 'localhost'
        && node.@artifactId == 'dummy-impl'
        && node.@oldVersion == '1.0'
        && node.@newVersion == '2.2' }.size() == 2)

assert (changes.dependencyUpdate.findAll { node -> node.@kind == 'property-update'
        && node.@groupId == 'localhost'
        && node.@artifactId == 'dummy-impl'
        && node.@classifier == 'classifier1'
        && node.@oldVersion == '1.0'
        && node.@newVersion == '2.2' }.size() == 1)
