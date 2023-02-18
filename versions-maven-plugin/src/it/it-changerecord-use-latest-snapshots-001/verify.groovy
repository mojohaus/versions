import groovy.xml.XmlSlurper

def changes = new XmlSlurper().parse( new File( basedir, 'target/versions-changes.xml' ) )
assert changes.dependencyUpdate.find { node -> node.@kind == 'dependency-update'
        && node.@groupId == 'localhost'
        && node.@artifactId == 'dummy-api'
        && node.@oldVersion == '1.0'
        && node.@newVersion == '1.9.1-SNAPSHOT' } != null
