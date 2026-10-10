def log = new File(basedir, "build.log").text

// dummy-maven-plugin 3.1 requires Maven 3.8.4, so its update is offered in that group only
assert log =~ /Require Maven 3\.8\.4 to use the following plugin updates:\s*\[INFO\]\s+localhost:dummy-maven-plugin \.+ 3\.0 -> 3\.1/
assert (log =~ /dummy-maven-plugin \.+ 3\.0 -> /).count == 1
