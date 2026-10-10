def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<versionProp>3.2-SNAPSHOT</versionProp>") : "versionProp has been changed which shouldn't happen."
assert buf.contains('<version>${versionProp}</version>') : "version entry has been changed which should not happen."
