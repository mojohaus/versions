def report = new File(basedir, "target/site/dependency-updates-report.html").getText("UTF-8")
        .replaceAll("<[^>]+>", " ")
        .replaceAll("&[^;]+;", " ")
        .replaceAll("\\s+", " ")

// the range stays as declared, and 3.0 is reported as lying outside it
assert report.contains("localhost dummy-api [1.1,3.0) compile jar 3.0")
assert report.contains("3.0 Latest Major * Outside allowed version range")
