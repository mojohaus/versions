def pom = new XmlSlurper()
        .parse( new File( basedir, 'target/property-updates-report.xml' ) )

assert pom.summary.usingLastVersion == 1
assert pom.summary.nextIncrementalAvailable == 1
assert pom.summary.nextMinorAvailable == 1

assert pom.properties.property.find { node -> node.propertyName == 'api-version' }
    .status == 'incremental available'
assert pom.properties.property.find { node -> node.propertyName == 'impl-version' }
        .status == 'minor available'
assert pom.properties.property.find { node -> node.propertyName == 'plugin-version' }
        .status == 'no new available'
