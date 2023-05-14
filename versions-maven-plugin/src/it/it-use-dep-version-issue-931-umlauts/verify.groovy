import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert project.dependencies.dependency.version == '1.0.1'
assert project.description == 'Wörter mit Umlauten'
