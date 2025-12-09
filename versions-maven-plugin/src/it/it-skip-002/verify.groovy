// Check that the log contains the skip message
def buildLog = new File(basedir, 'build.log')
assert buildLog.text.contains('Skipping execution')

// Verify that no dependency update information is shown
// When skipped, it shouldn't display any dependency analysis
assert !buildLog.text.contains('The following dependencies in Dependencies have newer versions')
