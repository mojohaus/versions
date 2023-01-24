def parentOutput = new File( basedir, "output1.txt").text
assert parentOutput =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/
assert parentOutput =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/
assert !(parentOutput =~ /\Qlocalhost:dummy-impl\E/)

def childOutput = new File( basedir, "child/output1.txt")
assert !childOutput.exists()

def parentOutput2 = new File( basedir, "output2.txt").text
assert parentOutput2 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q\u0024{dummyMavenPluginVersion}\E\s+->\s+\Q3.1\E/
assert parentOutput2 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q\u0024{dummyApiVersion}\E\s+->\s+\Q3.0\E/
