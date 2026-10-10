def buf = new File(basedir, "target/depDiffs.txt").getText("UTF-8")
assert buf.contains("junit.version") : "junit.version property reference not found"
assert buf.contains("4.13.1 -> 4.1") : "junit.version property update not found"
assert !buf.contains("another.property") : "another.property should not be in the report"
