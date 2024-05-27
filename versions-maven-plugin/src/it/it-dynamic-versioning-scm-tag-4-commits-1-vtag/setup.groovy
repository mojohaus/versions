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
exec('git tag 1.1.1')
exec('git tag')

testFile << 'content2'
exec('git add test.txt')
exec('git commit -m commit_no2')

testFile << 'content3'
exec('git add test.txt')
exec('git commit -m commit_no3')

testFile << 'content4'
exec('git add test.txt')
exec('git commit -m commit_no4')
