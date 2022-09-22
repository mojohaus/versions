report = new File( basedir, "target/site/parent-updates-report.html" ).text
        .replaceAll( "<[^>]+>", " " )
        .replaceAll( "&[^;]+;", " " )
        .replaceAll( "\\s+", " " )

assert report =~ /dummy-parent2 +1.0 +pom +3.1/

report = new File( basedir, "module/target/site/parent-updates-report.html" ).text
        .replaceAll( "<[^>]+>", " " )
        .replaceAll( "&[^;]+;", " " )
        .replaceAll( "\\s+", " " )

assert report =~ /dummy-parent3 +1.0.0 +pom/