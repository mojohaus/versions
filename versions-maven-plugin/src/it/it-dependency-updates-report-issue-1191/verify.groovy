import groovy.xml.XmlSlurper

def report = new XmlSlurper().parse(new File(basedir, "target/dependency-updates-report.xml"))
assert !(report
        .dependencyManagements
        .dependencyManagement
        .find { node -> node.artifactId == 'dummy-api' }.isEmpty())

