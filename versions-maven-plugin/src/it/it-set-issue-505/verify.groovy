assert new File( basedir, "pom.xml" ).text.contains( 'TEST' )
assert new File( basedir, "moduleA/pom.xml" ).text.contains( 'TEST' )
assert new File( basedir, "moduleA/moduleB/pom.xml" ).text.contains( 'TEST' )
