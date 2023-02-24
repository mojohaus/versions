import groovy.xml.XmlSlurper

def parentPom = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert parentPom.version == '1.2.1-SNAPSHOT'

def moduleA1 = new XmlSlurper().parse( new File( basedir, 'module-a1/pom.xml' ) )
assert moduleA1.parent.version == '1.2.1-SNAPSHOT'

def moduleA2 = new XmlSlurper().parse( new File( basedir, 'module-a2/pom.xml' ) )
assert moduleA2.parent.version == '1.2.1-SNAPSHOT'

