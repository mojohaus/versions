def csvChanges = new File(basedir, 'target/versions-changes.csv').text
assert csvChanges =~ "[^;]+;update-parent;[^;]+;PARENT_UPDATE;localhost;dummy-parent;1\\.0;3\\.0;;;"
