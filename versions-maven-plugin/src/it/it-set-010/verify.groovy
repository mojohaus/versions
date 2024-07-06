import groovy.xml.XmlSlurper

def pom = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
assert pom.version == '1.0'
