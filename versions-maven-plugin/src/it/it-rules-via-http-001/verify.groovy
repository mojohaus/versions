context.get('rules.server').stop(0)

def pom = new File(basedir, "pom.xml").getText("UTF-8")
assert pom.contains("<version>3.0.1-1</version>") : "The rule ignoring version-rules 3.0.1-1.1 was not applied"

def requests = new File(basedir, "requests.log").readLines()
assert requests.contains("GET 200 authenticated") : "The rules were not fetched with the credentials of the server: ${requests}"
