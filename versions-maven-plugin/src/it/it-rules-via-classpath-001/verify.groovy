def buf = new File(basedir, "build.log").getText("UTF-8")
def expectedMessage = "Resource \"classpath:///package/foo/bar/rule-set.xml\" not found in classpath."
assert buf.contains(expectedMessage) : "Build should have failed as the requested resource is not present."
