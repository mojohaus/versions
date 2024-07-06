import groovy.xml.XmlSlurper

def parentPom = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert parentPom.version == '1.0'

def child = new XmlSlurper().parse(new File(basedir, 'child/pom.xml'))
assert child.parent.version == '1.0'
