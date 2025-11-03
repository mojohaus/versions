def output = new File(basedir, "output.txt").text
assert output =~ /No dependencies in Dependencies have newer versions./
