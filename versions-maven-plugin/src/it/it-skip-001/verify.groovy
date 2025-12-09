import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse(new File(basedir, 'pom.xml'))
// Version should remain 1.0.0-SNAPSHOT since skip=true
assert project.version == '1.0.0-SNAPSHOT'

// Check that the log contains the skip message
def buildLog = new File(basedir, 'build.log')
assert buildLog.text.contains('Skipping execution')
