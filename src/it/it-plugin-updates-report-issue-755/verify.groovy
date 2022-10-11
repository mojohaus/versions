report = new File( basedir, "target/plugin-updates-report.xml" ).text.replaceAll( '\\s*', '' )

assert report.contains( '<major>3.0.0</major>' )
assert report.contains( '<minor>2.3</minor>' )
assert report.contains( '<incremental>2.2.2</incremental>' )
