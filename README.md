# MojoHaus Versions Maven Plugin - JGit PoC

This is an updated version of [versions-maven-plugin](http://www.mojohaus.org/versions-maven-plugin/)
with a jGit hack to perform commit based on properties update (`mvn versions:update-properties`)
 
 
### JGIT hack in

Introduced JGIT in in order to be able to produce branch containing commits for each property updated.

This to avoid dependabot and possible security breaches when using corporate network/credentials (even if it's safe for what I saw)
but each company has different needs.

- JGIT is created at start
- Creates a branch "dependencies-updates" in the local directory where you launch the plugin.
- Push commit each time a file is updated and adds to the message the link of the release notes/changelogs.
- Leave your local repository safe: do not ask for email/user/authentication... everything is local.
- Leave you the choice to rebase your commits and remove the unwanted.

#### Only a PoC for now

This is only a poc, tested locally for now
But the commits are created with the release notes link.

There is a few bugs to fix asap:
- code is not pretty
- commit only the pom file(s) (actually commit everything is not stashed!)
- The RN files need to be updated/completed / manage uppercase/lower/name
- fixup singleton
- only tested with `mvn versions:update-properties -Dmaven.version.rules=<file_location> -DgenerateBackupPoms=false`
- To test it, build it locally, then remove any 2.8.1 version in your .m2 folder. Then run the above command.

#### Disclaimer
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

VROM© 2021.
