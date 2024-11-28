def buildLog = new File( basedir, "build.log").text
assert buildLog =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+3\.0/
assert buildLog =~ /\Qlocalhost:dummy-impl\E\s*\.*\s*1\.2\s+->\s+2\.2/
assert !(buildLog =~ /\Qlocalhost:dummy-api-impl-bom-pom\E\s*\.*\s*1\.0\s+->\s+2\.0/)
