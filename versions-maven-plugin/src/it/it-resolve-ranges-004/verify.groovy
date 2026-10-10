import org.apache.maven.model.io.xpp3.MavenXpp3Reader

def read = { path -> new File(basedir, path).withReader { new MavenXpp3Reader().read(it) } }

def ranges = read("pom.xml").dependencyManagement.dependencies.findAll { it.version.contains(",") }
assert ranges.isEmpty() : "Unresolved dependency ranges: " + ranges.collect { it.managementKey + ":" + it.version }

def versioned = read("child/pom.xml").dependencies.findAll { it.version != null }
assert versioned.isEmpty() : "Declared versions (version should not be set): " + versioned.collect { it.managementKey + ":" + it.version }
