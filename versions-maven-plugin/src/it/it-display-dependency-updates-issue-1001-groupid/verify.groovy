def output = new File(basedir, "output.txt").text
assert output =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E\b/
