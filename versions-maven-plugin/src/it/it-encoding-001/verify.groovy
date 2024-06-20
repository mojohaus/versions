def pom = new File(basedir, "pom.xml").getText("ISO-8859-15")

// if pom.xml has not been modified, the test is not useful...
assert pom.contains('<version>2.0</version>')

assert pom.contains('<name>Modify a POM stored with ISO-8859-15 encoding (check euro symbol: \u20AC)</name>') :
        'Encoding not preserved while modifying pom.xml'
