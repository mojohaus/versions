def buildLog = new File(basedir, "build.log")

assert buildLog.text.contains("""
[INFO] The following dependencies in Dependencies have newer versions:
[INFO]   localhost:dummy-api ....................................... 1.0 -> 3.0
[INFO]   localhost:dummy-impl ...................................... 1.0 -> 2.2
[INFO] 
[INFO] ------------------------------------------------------------------------
""".replaceAll( "\n", System.lineSeparator() ) )

return true
