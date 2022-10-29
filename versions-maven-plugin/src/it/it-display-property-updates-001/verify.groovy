output = new File(basedir, "output1.txt").text
assert output =~ /\Q\u0024{api}\E\s*\.*\s*1\.0\s+->\s+2\.0\b/

output = new File(basedir, "output2.txt").text
assert ! ( output =~ /\Q\u0024{api}\E\s*\.*\s*1\.0\s+->\s+2\.0\b/ )
