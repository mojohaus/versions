// check that property was set

def mavenLogFile = new File(basedir, 'build.log')
assert mavenLogFile.exists() : "Maven log file does not exist"

// Read the log file
def logContent = mavenLogFile.text

// Define a pattern to match the version output
def versionPattern = /\[INFO\] Property 'revision' set to: (.+)/ 
def matcher = (logContent =~ versionPattern)
assert matcher.find() : "Version information not found in log file"

// Extract the version from the matched group
def actualVersion = matcher[0][1]

def expectedVersion = '0.0.1-1-SNAPSHOT'

assert actualVersion == expectedVersion : "Expected version '${expectedVersion}', but found '${actualVersion}'"
