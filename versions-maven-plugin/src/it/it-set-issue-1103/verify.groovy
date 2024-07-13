import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert project.version == '2'

def parent = new XmlSlurper().parse(new File(basedir, 'parent/pom.xml'))
assert parent.parent.version == '2'
assert parent.version == '100-SNAPSHOT'

def child = new XmlSlurper().parse(new File(basedir, 'parent/child/pom.xml'))
assert child.parent.version == '100-SNAPSHOT'

def grandchild = new XmlSlurper().parse(new File(basedir, 'parent/child/grandchild/pom.xml'))
assert grandchild.parent.version == '100-SNAPSHOT'
