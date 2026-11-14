def buf = new File(basedir, "build.log").text
assert !buf.contains("Duplicate key") : "a report plugin declared in two profiles of the parent is read once"
assert buf.contains("All plugins with a version specified are using the latest versions.") || buf.contains("The following plugin updates are available")
