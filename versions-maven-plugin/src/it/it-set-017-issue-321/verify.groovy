

def log = new File( basedir, "build.log" ).text
assert log.contains(' Illegal processing instruction target ("xml"); xml (case insensitive) is reserved by the specs.')
