def buf = new File(basedir, "build.log").text
assert !buf.contains("do not have their version specified") : "dummy-maven-plugin gets its version from the parent"
assert buf.contains("All plugins have a version specified.")
