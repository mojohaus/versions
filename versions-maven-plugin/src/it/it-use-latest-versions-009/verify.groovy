def project = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert project.parent.version == '3.0'
