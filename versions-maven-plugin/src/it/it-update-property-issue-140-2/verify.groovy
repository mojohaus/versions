def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionModule>1.2.3-SNAPSHOT</versionModule>") : "versionModule has been changed which should not happened."
assert buf.contains("<versionModuleTest>0.0.2.19</versionModuleTest>") : "versionModuleTest not updated to 0.0.2.19"
