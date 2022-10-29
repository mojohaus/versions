output = new File( basedir, 'target/site/plugin-updates-report.html' ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )
assert ! ( output =~ /\blocalhost dummy-maven-plugin 1.0 3.1\b/ )
assert output =~ /\blocalhost dummy-maven-plugin 1.0 3.0\b/
