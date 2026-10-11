def parent = new File(basedir, "pom.xml").getText("UTF-8")
assert parent.contains("<api>3.0</api>")

for (module in ["module1", "module2"]) {
    def pom = new File(basedir, module + "/pom.xml").getText("UTF-8")
    assert pom.contains('<version>${api}</version>')
    assert !pom.contains("<api>")
}

// the second module finds the property already updated
def log = new File(basedir, "build.log").text
assert log.count('Updated ${api} from 1.0 to 3.0') == 1
assert log.contains('Property ${api}: Leaving unchanged as 3.0')
