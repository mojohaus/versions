def project = new XmlSlurper()
        .parse( new File( basedir, 'pom.xml' ) )

assert project.dependencyManagement.dependencies.'*'.size() == 1
assert project.dependencyManagement.dependencies.dependency.version == '1.0.0-SNAPSHOT'
