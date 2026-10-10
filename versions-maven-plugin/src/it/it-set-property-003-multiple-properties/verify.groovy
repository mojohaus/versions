def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionModule>9.5.0-20170604.123456-2</versionModule>") : "versionModule has not been changed which should not happen."
assert buf.contains("<versionModuleTest>9.5.0-20170604.123456-2</versionModuleTest>") : "versionModuleTest has not been changed which should not happen."
