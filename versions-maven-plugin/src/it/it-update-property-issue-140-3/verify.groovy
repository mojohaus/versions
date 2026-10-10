def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionModule>0.0.2.19</versionModule>") : "versionModule has not been updated as expected."
assert buf.contains("<versionModuleTest>1.2.3-SNAPSHOT</versionModuleTest>") : "versionModuleTest has been changed which should not happened."
