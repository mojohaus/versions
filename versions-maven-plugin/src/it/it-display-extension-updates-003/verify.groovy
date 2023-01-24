def output1 = new File( basedir, "output1.txt").text
assert !( output1 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/ )
assert output1 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/

def output2 = new File( basedir, "output2.txt").text
assert output2 =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.1\E/
assert !( output2 =~ /\Qlocalhost:dummy-api\E\s*\.*\s*\Q1.0\E\s+->\s+\Q3.0\E/ )
