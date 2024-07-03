import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert project.version == '2.0'
assert project.dependencyManagement.dependencies.dependency[0].version == '2.0'

def child = new XmlSlurper().parse(new File(basedir, 'child/pom.xml'))
assert child.parent.version ==  '2.0'
assert child.version ==  '2.0'

def child2 = new XmlSlurper().parse(new File(basedir, 'child/pom.xml'))
assert child2.parent.version ==  '2.0'
assert child2.version ==  '2.0'
