def buf = new File(basedir, "target/depDiffs.txt").getText("UTF-8")
assert !buf.contains("junit.version") : "junit.version property reference should not be found"
assert !buf.contains("4.1 -> 4.1") : "junit.version property update should not be found"
