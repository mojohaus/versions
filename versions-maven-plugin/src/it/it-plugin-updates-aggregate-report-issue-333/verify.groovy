output = new File( basedir, 'target/site/plugin-updates-aggregate-report.html' ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

// Plugin management
assert !( output =~ 'This project does not declare any plugins in a build/pluginManagement section.' )
assert ( output =~ 'org.apache.maven.plugins maven-clean-plugin 3.0.0' )

// Plugins
assert !( output =~ 'This project does not declare any plugins in a build/plugins section.' )
assert ( output =~ 'org.apache.maven.plugins maven-clean-plugin 3.1.0' )
assert ( output =~ 'org.apache.maven.plugins maven-clean-plugin 3.2.0' )
