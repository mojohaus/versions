dependencyUpdatesReport = new File( basedir, "target/site/dependency-updates-report.html" )

assert dependencyUpdatesReport.exists(  )

// some basic (=not comprehensive) checks on the contents of the dependency report
assert dependencyUpdatesReport.text =~ /\b1\.1\.0-2\b/  // current version of the dependency
assert dependencyUpdatesReport.text =~ /\b3\.0\b/       // latest major available version

pluginUpdatesReport = new File( basedir, "target/site/plugin-updates-report.html" )

assert pluginUpdatesReport.exists(  )

// some basic (=not comprehensive) checks on the contents of the plugin report
assert pluginUpdatesReport.text =~ /\b1\.0\b/        // current version of the plugin
assert pluginUpdatesReport.text =~ /\b3\.1\b/        // latest major available version