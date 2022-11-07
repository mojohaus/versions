def project = new XmlSlurper().parse( new File( basedir, 'pom.xml' ) )
assert project.parent.version == '2.0'
