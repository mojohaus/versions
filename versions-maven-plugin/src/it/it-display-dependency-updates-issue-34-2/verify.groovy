
def buildLog = new File( basedir, "build.log")

assert buildLog.text.contains( '[INFO] The following dependencies in Dependency Management have newer versions:')
assert buildLog.text.contains( '[INFO]   localhost:dummy-api ....................................... 1.0 -> 3.0')

assert buildLog.text.contains( '[INFO] The following dependencies in Dependencies have newer versions:' )
assert buildLog.text.contains( '[INFO]   localhost:dummy-api ....................................... 2.0 -> 3.0' )

assert buildLog.text.contains( '[INFO] The following dependencies in Plugin Management Dependencies have newer versions:' )
assert buildLog.text.contains( '[INFO]   localhost:dummy-api ....................................... 1.2 -> 3.0' )

assert !buildLog.text.contains( '[INFO] The following dependencies in Plugin Dependencies have newer versions:')
assert !buildLog.text.contains( '[INFO]   localhost:dummy-api ....................................... 1.1 -> 3.0' )

assert buildLog.text.contains( '[INFO] BUILD SUCCESS' )

return true
