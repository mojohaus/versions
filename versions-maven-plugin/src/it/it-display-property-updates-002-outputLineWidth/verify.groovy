def output = new File(basedir, "dependencyUpdate.txt")
assert output.exists()
assert output.text.contains('${api} ........................................................................................... 1.0 ->') : "displayTerminalWidth parameter not respected"
