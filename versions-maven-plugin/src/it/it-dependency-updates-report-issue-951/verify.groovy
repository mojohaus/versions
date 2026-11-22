def report = new File(basedir, 'target/reports/dependency-updates-report.html')
assert report.isFile() : 'no report was generated'
def text = report.getText('UTF-8').replaceAll(/<[^>]*>/, ' ').replaceAll(/\s+/, ' ')

// the range from the managed dependency is reported for the managed entry and for the dependency without a version
assert text =~ /Dependency Management .*dummy-api \[1\.0,2\.3\] test jar 3\.0/ : 'managed dependency missing from the report'
assert text =~ /Dependencies Status .*dummy-api \[1\.0,2\.3\] compile jar 3\.0/ : 'dependency missing from the report'
