def output = new File(basedir, "output.txt").text
assert output =~ /\Qlocalhost:dummy-parent4\E\s*\.*\s*\Q70\E\s+->\s+\Q71-SNAPSHOT\E\b/
