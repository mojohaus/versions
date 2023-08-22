
import groovy.xml.XmlSlurper

def project = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert project.parent.version == '70'


def buildLog = new File( basedir, "build.log")

assert buildLog.text.contains( '[INFO] The parent project is the latest version')
