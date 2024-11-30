def output = new File( basedir, 'target/reports/property-updates-aggregate-report.html' ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

assert output =~ '\\$\\{version.dummy-lib} 1.1.1.1'
assert output =~ '\\$\\{version.dummy-lib} 2.12.0.0'
