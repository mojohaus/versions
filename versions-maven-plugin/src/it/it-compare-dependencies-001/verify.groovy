def buf = new File(basedir, "target/depDiffs.txt").getText("UTF-8")
assert buf.contains("2.0.10 -> 2.0.9") : "Version diff in maven artifact not found"
