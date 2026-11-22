def log = new File(basedir, 'build.log').getText('UTF-8').replace('\r\n', '\n')
assert log.contains('Processing change of localhost:child:1.0 -> 2.0') : 'the change of the child was not processed'

// the child declares no version of its own and the parent reference is not the child's version
def root = new File(basedir, 'pom.xml').getText('UTF-8').replace('\r\n', '\n')
assert root.contains('<version>1.0</version>') : 'root version changed'
assert root =~ /(?s)<artifactId>dummy-api<\/artifactId>\s*<version>1\.1<\/version>/ : 'managed dependency changed'
def child = new File(basedir, 'child/pom.xml').getText('UTF-8').replace('\r\n', '\n')
assert child =~ /(?s)<parent>.*<version>1\.0<\/version>.*<\/parent>/ : 'parent reference changed'
assert !child.contains('2.0') : 'child was given a version'
