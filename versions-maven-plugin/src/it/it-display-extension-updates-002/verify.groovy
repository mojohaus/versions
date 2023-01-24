def output1 = new File( basedir, "output1.txt").text
assert output1 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/
assert output1 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/

def output2 = new File( basedir, "output2.txt")
assert !output2.exists()

def output3 = new File( basedir, "output3.txt").text
assert output3 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/
assert !( output3 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/ )
