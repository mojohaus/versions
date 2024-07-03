import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert project.version == '2.0'

def child = new XmlSlurper().parse(new File(basedir, 'child/pom.xml'))
assert child.parent.version == '2.0'
