def buf = new File(basedir, "target/depDiffs.txt").getText("UTF-8")
assert buf.contains("2.0.10 -> 2.0.9") : "Version diff in maven artifact not found. it should be processed because its scope is compile"
assert !buf.contains("4.0 -> 4.1") : "Version diff in junit artifact found. it should be excluded because its scope is test"
