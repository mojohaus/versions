import groovy.xml.XmlSlurper

def parentPom = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert parentPom.version == '1.2.1-SNAPSHOT'

def moduleA1 = new XmlSlurper().parse(new File(basedir, 'module-a1/pom.xml'))
assert moduleA1.parent.version == '1.2.1-SNAPSHOT'
assert moduleA1.version == '2.0.7-SNAPSHOT' // due to updateMatchingVersions=false

def moduleA2 = new XmlSlurper().parse(new File(basedir, 'module-a2/pom.xml'))
assert moduleA2.parent.version == '1.2.1-SNAPSHOT'
assert moduleA2.version == '1.2.0' // due to updateMatchingVersions=false

def moduleA3 = new XmlSlurper().parse(new File(basedir, 'module-a3/pom.xml'))
assert moduleA3.parent.version == '1.2.1-SNAPSHOT'
assert moduleA3.version == ''
