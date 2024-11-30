def output = new File( basedir, 'target/reports/plugin-updates-aggregate-report.html' ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

// Plugin management
assert !(output =~ 'This project does not declare any plugins in a build/pluginManagement section.')
assert output =~ 'localhost dummy-maven-plugin 1.0'

// Plugins
assert !(output =~ 'This project does not declare any plugins in a build/plugins section.')
assert output =~ 'localhost dummy-maven-plugin 2.0'
assert output =~ 'localhost dummy-maven-plugin 3.0'
