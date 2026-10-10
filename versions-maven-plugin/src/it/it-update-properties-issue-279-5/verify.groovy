def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionProp>1.1.0-2</versionProp>") : "versionProp has not been changed to 1.1.0-2 which shouldn't happen."
assert buf.contains('<version>${versionProp}</version>') : "version entry has been changed which should not happen."
