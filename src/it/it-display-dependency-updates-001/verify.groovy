output = new File(basedir, "output1.txt").text
assert output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+3\.0\b/

output = new File(basedir, "output2.txt").text
assert ! ( output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+3\.0\b/ )
assert output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*1\.1\s+->\s+2\.1\b/
