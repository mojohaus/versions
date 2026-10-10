def pom = new File(basedir, "module-a2/pom.xml").readLines("UTF-8").join(" ")
assert pom =~ /\Q<parent>\E.*\Q<version>1.0<\/version>\E.*\Q<\/parent>\E/ : "Parent was updated incorrectly"
assert pom =~ /\Q<dependency>\E.*\Q<version>1.1-SNAPSHOT<\/version>\E.*\Q<\/dependency>\E/ : "Dependency with module-a1 was not updated correctly"
