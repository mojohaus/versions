dependencyUpdatesReport = new File( basedir, "target/site/dependency-updates-report.html" ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

// some basic (=not comprehensive) checks on the contents of the dependency report
assert dependencyUpdatesReport =~ /\b1\.1\.0-2\b/  // current version of the dependency
assert dependencyUpdatesReport =~ /\b3\.0\b/       // latest major available version

pluginUpdatesReport = new File( basedir, "target/site/plugin-updates-report.html" ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

// some basic (=not comprehensive) checks on the contents of the plugin report
assert pluginUpdatesReport =~ /\b1\.0\b/        // current version of the plugin
assert pluginUpdatesReport =~ /\b3\.1\b/        // latest major available version