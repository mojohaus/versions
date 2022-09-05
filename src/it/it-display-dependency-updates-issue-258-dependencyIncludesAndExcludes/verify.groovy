output = new File(basedir, "output1.txt").text
assert output =~ /localhost:dummy-api/
assert output !=~ /localhost:dummy-impl/

output = new File(basedir, "output2.txt").text
assert output !=~ /localhost:dummy-api/
assert output =~ /localhost:dummy-impl/

output = new File(basedir, "output3.txt").text
assert output =~ /localhost:dummy-impl/
assert output !=~ /localhost:dummy-parent2/

output = new File(basedir, "output4.txt").text
assert output !=~ /localhost:dummy-impl/
assert output =~ /localhost:dummy-parent2/

output = new File(basedir, "output5.txt")
assert !output.exists(  )
