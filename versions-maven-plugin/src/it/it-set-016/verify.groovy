import groovy.xml.XmlSlurper

def pom = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert pom.version == '1.1-SNAPSHOT'

def child = new XmlSlurper().parse(new File(basedir, 'child/pom.xml'))
assert child.parent.version == '1.1-SNAPSHOT'
