
propertyUpdatesReport = new File( basedir, "target/reports/property-updates-report.html" ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

assert ! ( propertyUpdatesReport =~ /\b1\.1\.0-2\b/ )
assert ! ( propertyUpdatesReport =~ /\b1\.1\.1-2\b/ )
// Summary
assert propertyUpdatesReport =~ / \[1\.1\.2,3\.0\] 1\.1\.3 1\.3 3/
// Detail
assert propertyUpdatesReport =~ /Newer versions 1\.1\.3 Latest Incremental/
assert propertyUpdatesReport =~ /\b1\.2\.2 1\.3 Latest Minor 2\.0 2\.1 3\.0\b/

dependencyUpdatesReport = new File( basedir, "target/reports/dependency-updates-report.html" ).text
        .replaceAll( '<[^>]+>', ' ' )
        .replaceAll( '&[^;]+;', ' ' )
        .replaceAll( '\\s+', ' ' )

assert ! ( dependencyUpdatesReport =~ /\b1\.1\.0-2\b/ )
assert ! ( dependencyUpdatesReport =~ /\b1\.1\.1-2\b/ )
// Summary
assert propertyUpdatesReport =~ / \[1\.1\.2,3\.0\] 1\.1\.3 1\.3 3/
// Detail
assert propertyUpdatesReport =~ /Newer versions 1\.1\.3 Latest Incremental/
assert propertyUpdatesReport =~ /\b1\.2\.2 1\.3 Latest Minor 2\.0 2\.1 3\.0\b/
