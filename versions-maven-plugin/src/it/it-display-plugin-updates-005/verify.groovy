output = new File(basedir, "output.txt").text
assert output =~ /\Qmaven-deploy-plugin\E\s*\.*\s*\Q2.3 ->\E/
