def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionModule>1.2.3-SNAPSHOT</versionModule>") : "versionModule has been changed which should not happen."
assert buf.contains("<versionModuleTest>9.5.0-20170604.123223-2</versionModuleTest>") : "versionModuleTest has not been changed which should not happen."
