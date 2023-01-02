def output = new File(basedir, "output.txt").text
assert output =~ /\Qlocalhost:dummy-parent2\E\s*\.*\s*\Q3.1\E\s*$/
