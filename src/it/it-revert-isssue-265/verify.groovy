assert new File( basedir, "aggregate/pom.xml" ).text =~ /OLD/
assert !new File( basedir, "aggregate/pom.xml.versionsBackup" ).exists()

assert new File( basedir, "module-a/pom.xml" ).text =~ /OLD/
assert !new File( basedir, "module-a/pom.xml.versionsBackup" ).exists()

assert new File( basedir, "module-b/pom.xml" ).text =~ /OLD/
assert !new File( basedir, "module-b/pom.xml.versionsBackup" ).exists()
