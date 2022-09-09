output = new File(basedir, "output1.txt").text
assert ! ( output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+3\.0\b/ )
assert output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+2\.1\b/

output = new File(basedir, "output2.txt").text
assert ! ( output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+2\.1\b/ )
assert output =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*1\.0\s+->\s+2\.0\b/
