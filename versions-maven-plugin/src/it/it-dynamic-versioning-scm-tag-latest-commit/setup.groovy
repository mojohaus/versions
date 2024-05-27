void exec(String command) {
    def proc = command.execute(null, basedir)
    proc.consumeProcessOutput(System.out, System.out)
    proc.waitFor()
    assert proc.exitValue() == 0 : "Command '${command}' returned status: ${proc.exitValue()}"
}

def testFile = new File(basedir, 'test.txt')
testFile << 'content'

exec('git init')
exec('git add test.txt')
exec('git commit -m initial-commit')
exec('git tag v1.1.1')