import groovy.xml.XmlSlurper

def report = new XmlSlurper().parse(new File(basedir, "target/plugin-updates-report.xml"))
assert report.namespaceURI() == "https://www.mojohaus.org/VERSIONS/PLUGIN-UPDATES-REPORT/2.0.0"
assert report.plugins.plugin.findAll { it.artifactId == "dummy-maven-plugin" }*.currentVersion*.text().sort() == ["2.0", "3.0"]
assert report.pluginManagements.pluginManagement.find { it.artifactId == "dummy-maven-plugin" }.currentVersion == "1.0"
assert report.plugins.plugin.findAll { it.artifactId == "dummy-maven-plugin" }.every { it.lastVersion == "3.1" }
def aggregate = new XmlSlurper().parse(new File(basedir, "target/plugin-updates-aggregate-report.xml"))
assert aggregate.namespaceURI() == report.namespaceURI()
assert aggregate.plugins.plugin.findAll { it.artifactId == "dummy-maven-plugin" }*.currentVersion*.text().sort() == ["2.0", "2.1", "3.0"]
assert aggregate.pluginManagements.pluginManagement.find { it.artifactId == "dummy-maven-plugin" }.currentVersion == "1.0"
def html = new File(basedir, "target/reports/plugin-updates-report.html").text.replaceAll(/<[^>]+>/, " ").replaceAll(/\s+/, " ")
assert html.contains("dummy-maven-plugin 2.0")
assert html.contains("dummy-maven-plugin 3.0")
assert html.contains("dummy-api")
def cli = new File(basedir, "cli.txt").text
assert cli.contains("Require Maven 3.8.4 to use the following plugin updates:")
assert cli.contains("1.0 -> 3.1")
return true
