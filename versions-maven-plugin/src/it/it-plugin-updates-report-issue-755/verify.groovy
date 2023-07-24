report = new File( basedir, "target/plugin-updates-report.xml" ).text.replaceAll( '\\s*', '' )


assert report.contains( '<minor>2.1</minor>' )
assert report.contains( '<major>3.1</major>' )
