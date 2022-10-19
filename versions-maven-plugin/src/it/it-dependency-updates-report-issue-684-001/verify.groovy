output = new File( basedir, "target/site/dependency-updates-report.html" ).text
assert ! ( output =~ /\b3.0\b/ )
