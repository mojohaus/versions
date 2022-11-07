def project = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert project.parent.version == '1.0.0'
