def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionModule>1.2.3-SNAPSHOT</versionModule>") : "versionModule has been changed which should not happened."
assert buf.contains("<versionModuleTest>1.2.3-SNAPSHOT</versionModuleTest>") : "versionModuleTest has been changed which should not happen."
