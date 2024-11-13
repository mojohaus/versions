def output = (1..3).collect{new File( basedir, "output." + it + ".txt").text.replaceAll( "(\n|\r)", " " ) }

assert output[0] =~ /localhost:dummy-api \.* 1.1 -> 3.0/
assert !(output[0] =~ /localhost:dummy-impl \.* 1.2 -> 2.2/)
assert output[0] =~ /localhost:dummy-api-impl-bom-pom \.* 1.0 -> 2.0/

assert output[1] =~ /localhost:dummy-api \.* 1.1 -> 3.0/
assert output[1] =~ /localhost:dummy-impl \.* 1.2 -> 2.2/
assert !(output[1] =~ /localhost:dummy-api-impl-bom-pom \.* 1.0 -> 2.0/)

assert output[2] =~ /localhost:dummy-api.*\b1.1\b.*\Q(managed by localhost:dummy-api-impl-bom-pom:1.0)\E.*\b3.0\b/
assert output[2] =~ /localhost:dummy-impl.*\b1.2\b.*\Q(managed by localhost:dummy-api-impl-bom-pom:1.0)\E.*\b2.2\b/
assert !(output[2] =~ /localhost:dummy-api-impl-bom-pom \.* 1.0 -> 2.0/)
