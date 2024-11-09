output = new File( basedir, "target/reports/dependency-updates-report.html" ).text
assert ! ( output =~ /\b3.0\b/ )
