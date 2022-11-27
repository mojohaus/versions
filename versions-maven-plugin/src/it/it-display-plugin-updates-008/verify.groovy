def buildLog = new File( basedir, "build.log" ).text

m1 = buildLog =~ /Require Maven 3\.8\.4 to use the following plugin updates:/
m2 = buildLog =~ /localhost:dummy-maven-plugin *\.*\s*\b1\.0 -> 3\.1\b/

assert m1 && m2 && m1.start() < m2.start()