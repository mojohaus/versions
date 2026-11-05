context.get('rules.server').stop(0)

def buf = new File(basedir, "build.log").getText("UTF-8")
assert buf =~ /Could not load specified rules from http:\/\/localhost:\d+\/rules\/rule-set\.xml/ : "The failure does not name the rules URI"

def requests = new File(basedir, "requests.log").readLines()
assert requests.any { it.startsWith("GET 401") } : "The server was never challenged: ${requests}"
assert !requests.any { it.contains("200") } : "The rules must not be served to wrong credentials: ${requests}"
