import groovy.xml.XmlSlurper

def changes = new XmlSlurper().parse( new File( basedir, 'target/versions-changes.xml' ) )
assert changes.dependencyUpdate.find { node -> node.@kind == 'dependency-update'
        && node.@groupId == 'localhost'
        && node.@artifactId == 'dummy-api'
        && node.@oldVersion == '1.1.1-2'
        && node.@newVersion == '3.0' } != null
