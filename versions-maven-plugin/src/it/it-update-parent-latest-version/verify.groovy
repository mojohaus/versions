import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert project.parent.version == '70'
