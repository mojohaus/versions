def report = new File(basedir, "target/site/dependency-updates-report.html").getText("UTF-8")
        .replaceAll("<[^>]+>", " ")
        .replaceAll("&[^;]+;", " ")
        .replaceAll("\\s+", " ")
assert report.contains("localhost dummy-api 1.1 jar 1.1.0-2 1.1.3 1.3 3.0") : "Did not pick up updated in Dependency Management section"
assert report.contains("localhost dummy-impl 1.1 jar 1.4 2.2") : "Did not pick up updated in Dependency Management section"
assert report.contains("This project does not declare any dependencies.") : "Dependency Management should have processed"
