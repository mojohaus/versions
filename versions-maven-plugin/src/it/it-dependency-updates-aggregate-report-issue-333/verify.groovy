def output = new File( basedir, 'target/reports/dependency-updates-aggregate-report.html' ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

// Dependency management
assert !( output =~ 'This project does not declare any dependencies in a dependencyManagement section.' )
assert output =~ 'localhost dummy-api 1.1 jar'
assert output =~ 'localhost dummy-impl 1.1 jar'
assert output =~ 'test.bigversion dummy-lib 2.0.0.0 jar'

// Dependencies
assert !(output =~ 'This project does not declare any dependencies.')
assert output =~ 'test.bigversion dummy-lib 3.0.0.0-SNAPSHOT compile jar'
assert output =~ 'test.package module 0.0.2.19 compile jar'
