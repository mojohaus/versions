def output = new File(basedir, "output.txt").text
assert output.contains('localhost:dummy-api ............................................................................... 1.0 ->')