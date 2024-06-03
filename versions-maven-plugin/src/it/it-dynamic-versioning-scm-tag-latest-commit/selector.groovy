try {
    // smoke test if we have a needed tools
    def gitVersion = "git --version".execute()
    gitVersion.consumeProcessOutput(System.out, System.out)
    gitVersion.waitFor()
    return gitVersion.exitValue() == 0
} catch (Exception e) {
    // some error occurs - we skip a test
    return false
}
