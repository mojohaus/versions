# MojoHaus Versions Maven Plugin - JGit PoC

This is an updated version of [versions-maven-plugin](http://www.mojohaus.org/versions-maven-plugin/)
with a jGit hack to perform commit based on properties update (`mvn versions:update-properties`)
 
 
### JGIT

Introduced JGIT in in order to be able to produce a branch containing the commits for each property updated when using the maven plugin.

As I tested Renovate bot and it seems the security around credentials and git is not following our workflow,
I'd choose JGIT to manage the local commits.

#### How it works?

- Compile the project and make sure there is no upper version of the Versions Maven Plugin in your .m2 folder
- Run the command:
`mvn versions:update-properties -Dmaven.version.rules=file:///path_to_rules_file -DgenerateBackupPoms=false`

Once executed:
- it will create a branch named "dependencies-updates" in the local directory where you launched the plugin.
- create a commit for each properties updated. If the release notes are located in the files, it creates a commit message containing the link to the release notes (which match the version update) or changelogs.


- It leaves your local repository safe: do not ask for email/user/authentication... everything is local.
- It leaves you the choice to rebase your commits and choose what updates you want to keep or not.

#### Only a PoC

This is only a poc, tested locally for now and working ok even if there is a few issues:
- code is not pretty
- commit only the pom file(s) (actually commit everything is not stashed!)
- The RN files need to be updated/completed / manage uppercase/lower/name in a more flexible way.
- fixup singleton
- only tested with `mvn versions:update-properties -Dmaven.version.rules=<file_location> -DgenerateBackupPoms=false`

#### Disclaimer
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

VROM© 2021.
