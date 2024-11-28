def output = new File(basedir, "child/target/dependency-updates-report.xml").text
assert !output.contains("<artifactId>dummy-api</artifactId>")
