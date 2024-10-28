package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.io.File;
import java.io.IOException;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.maven.artifact.versioning.ComparableVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;

/**
 * Maven plugin that uses SCM/VCS to enable dynamic versioning based on your
 * version control system. Goal will set the version in a property.
 *
 * @author Jimisola Laursen
 * @since 2.17.0
 */
@Mojo(name = "use-dynamic-version-from-scm", defaultPhase = LifecyclePhase.INITIALIZE)
public class DynamicVersioningSCMPlugin extends AbstractMojo {

    /**
     * The Maven Project Object
     *
     * @since 2.17.0
     */
    @Parameter(defaultValue = "${project}", readonly = true, required = true)
    private MavenProject project;

    /**
     * The name of the property that will contain the resolved version.
     *
     * @since 2.17.0
     */
    @Parameter(property = "propertyName", defaultValue = "revision")
    protected String propertyName;

    /**
     * Whether the SNAPSHOT qualifier shall be apppended or not.
     *
     * @since 2.17.0
     */
    @Parameter(property = "appendSnapshot", defaultValue = "true")
    protected boolean appendSnapshot;

    /**
     * Use this version instead of resolving from SCM tag information.
     *
     * @since 2.17.0
     */
    @Parameter(property = "useVersion")
    protected String useVersion;

    /**
     * The default version used when SCM repository has no commit or no version
     * tag.
     *
     * @since 2.17.0
     */
    @Parameter(property = "defaultVersion", defaultValue = "0.0.1")
    protected String defaultVersion;

    // standard semantic versioning with an optional 'v' prefix
    protected static final Pattern TAG_VERSION_PATTERN = Pattern.compile("refs/tags/(?:v)?((\\d+\\.\\d+\\.\\d+)(.*))");

    // created to mitigate LG_LOST_LOGGER_DUE_TO_WEAK_REFERENCE
    private static final Logger JGIT_LOGGER = Logger.getLogger("org.eclipse.jgit");

    public void execute() throws MojoExecutionException {
        // limit JGits excessive logging
        JGIT_LOGGER.setLevel(Level.INFO);

        VersionInformation vi;

        Optional<String> mayBeVersion = Optional.ofNullable(useVersion);

        if (mayBeVersion.isPresent()) {
            vi = new VersionInformation(mayBeVersion.get());
        } else {
            vi = getVersionFromSCM();
        }

        project.getProperties().setProperty(propertyName, vi.toString());
        getLog().info("Property '" + propertyName + "' set to: "
                + project.getProperties().getProperty(propertyName));
    }

    /**
     * Returns the resolved version based on SCM tag information for use with Maven
     * CI.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException Something wrong with
     *                                                        the
     *                                                        plugin itself
     */
    protected VersionInformation getVersionFromSCM() throws MojoExecutionException {
        // check for repository
        try (Repository repository = new FileRepositoryBuilder()
                .setGitDir(new File(".git"))
                .readEnvironment() // scan environment GIT_* variables
                .findGitDir() // scan up the file system tree
                .build(); ) {

            if (repository.getDirectory() == null) {
                throw new MojoExecutionException("Directory is not an SCM repository.");
            }

            // check for latest commit
            RevCommit latestCommit = getLatestCommit(repository);

            return getVersionFromCommit(repository, latestCommit);
        } catch (IOException e) {
            throw new MojoExecutionException("Error reading Git information.", e);
        }
    }

    protected RevCommit getLatestCommit(Repository repository) throws MojoExecutionException {
        try (RevWalk revWalk = new RevWalk(repository)) {
            ObjectId head = repository.resolve("HEAD");

            if (head == null) {
                throw new MojoExecutionException("SCM repo has no head/commits.");
            }

            return revWalk.parseCommit(head);
        } catch (IOException e) {
            throw new MojoExecutionException("SCM repo most likely has no commits.", e);
        }
    }

    protected VersionInformation getVersionFromCommit(Repository repository, RevCommit latestCommit)
            throws MojoExecutionException {

        try (Git git = Git.wrap(repository)) {

            List<String> versionTags = getVersionedTagsForCommit(git, latestCommit);

            Optional<VersionInformation> ovi = findHighestVersion(versionTags);

            // latest commit has version tag(s), we use the highest one
            if (ovi.isPresent()) {
                return ovi.get();
            }

            Iterable<RevCommit> commits = git.log().call();
            int count = 0;
            for (RevCommit commit : commits) {
                count++;

                versionTags = getVersionedTagsForCommit(git, commit);

                ovi = findHighestVersion(versionTags);

                if (ovi.isPresent()) {
                    VersionInformation vi = ovi.get();

                    vi.setPatch(vi.getPatch() + 1);
                    vi.setBuildNumber(count);

                    return addSnapshotQualifier(vi);
                }
            }

            // no version tags in repository
            return addSnapshotQualifier(new VersionInformation(defaultVersion + "-" + count));

        } catch (GitAPIException e) {
            throw new MojoExecutionException("Error reading Git information.", e);
        }
    }

    protected Optional<VersionInformation> findHighestVersion(List<String> versionTags) {
        Optional<String> highestVersionString = versionTags.stream().max(new VersionComparator());

        return highestVersionString.map(VersionInformation::new);
    }

    protected List<String> getVersionedTagsForCommit(Git git, RevCommit commit) throws GitAPIException {
        // get tags directly associated with the commit
        return git.tagList().call().stream()
                .filter(tag -> tag.getObjectId().equals(commit.getId()))
                .map(Ref::getName)
                .filter(tagName -> {
                    Matcher matcher = TAG_VERSION_PATTERN.matcher(tagName);
                    return matcher.matches() && matcher.groupCount() > 0;
                })
                .map(tagName -> {
                    Matcher matcher = TAG_VERSION_PATTERN.matcher(tagName);
                    matcher.matches();
                    return matcher.group(1);
                })
                .collect(Collectors.toList());
    }

    protected VersionInformation addSnapshotQualifier(VersionInformation vi) {
        if (appendSnapshot) {
            vi.setQualifier("SNAPSHOT");
        }

        return vi;
    }

    protected static class VersionComparator implements Comparator<String> {

        @Override
        public int compare(String version1, String version2) {
            return new ComparableVersion(version1).compareTo(new ComparableVersion(version2));
        }
    }
}
