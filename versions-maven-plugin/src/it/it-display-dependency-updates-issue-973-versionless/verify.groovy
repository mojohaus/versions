def output = new File(basedir, "output.txt").text
def matcher = output =~ /\Qlocalhost:dummy-api\E.*->\s+\Q3.0\E\b/
assert matcher.find() : "localhost:dummy-api should have been bumped to version 3.0"
assert !matcher.find() : "localhost:dummy-api may only appear once in the updates list"
