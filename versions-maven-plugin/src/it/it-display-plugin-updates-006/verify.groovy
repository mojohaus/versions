// because no minimum Maven version is not specified for the project, multiple
// possible updates for maven-deploy-plugin should be listed
//
// previously, the minimum Maven version for versions-maven-plugin was assumed due to #996
// which caused the plugin to only output the updates for the minimum version for versions-maven-plugin

def output = new File(basedir, "output.txt").text
assert output.contains('All plugins with a version specified are using the latest versions.')
assert output =~ /\Qmaven-deploy-plugin\E\s*\.*\s*\Q2.3 -> 2.4\E/
assert output =~ /\Qmaven-deploy-plugin\E\s*\.*\s*\Q2.3 -> 2.8.1\E/
assert output =~ /\Qmaven-deploy-plugin\E\s*\.*\s*\Q2.3 -> 2.8.2\E/
assert output =~ /\Qmaven-deploy-plugin\E\s*\.*\s*\Q2.3 -> 3.0.0-M2\E/
assert output =~ /\Qmaven-deploy-plugin\E\s*\.*\s*\Q2.3 -> 3.1.1\E/
