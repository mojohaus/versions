report = new File( basedir, "target/dependency-updates-report.xml" ).text.replaceAll( '\\s*', '' )

assert report.contains( '<incremental>1.0.1</incremental>' )
assert report.contains( '<minor>1.1</minor>' )
assert report.contains( '<major>2.0</major>' )
