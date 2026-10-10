// all 1.0 references to reactor projects are updated to 1.1-SNAPSHOT, including parents
for (module in ["the-grandparent", "the-parent", "module-a1", "module-a2"]) {
    def pom = new File(basedir, "$module/pom.xml").getText("UTF-8")
    assert !pom.contains("<version>1.0</version>") : "version 1.0 not bumped to 1.1-SNAPSHOT in $module"
}
