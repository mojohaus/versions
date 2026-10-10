import org.apache.maven.model.io.xpp3.MavenXpp3Reader

def parent = new File(basedir, "pom.xml").withReader { new MavenXpp3Reader().read(it) }.parent
assert parent.version == "2.0" : "Unresolved parent range: " + parent.id
