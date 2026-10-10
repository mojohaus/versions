def buf = new File(basedir, "build.log").getText("UTF-8")
def expectedMessage = "Resource \"classpath:///package/foo/bar/rules.xml\" not found in classpath."
assert !buf.contains(expectedMessage) : "File resource rules.xml should have been found in the classpath."
