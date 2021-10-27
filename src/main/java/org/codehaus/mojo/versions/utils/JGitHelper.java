package org.codehaus.mojo.versions.utils;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.FileSystems;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.RefAlreadyExistsException;

/**
 * This class is reponsible to initialise JGit with the given repo.
 * It does create a branch name in the gitinfo.property and where all the commits will
 * be pushed.
 */
public class JGitHelper {
    private static JGitHelper INSTANCE;
    private Git git;

    private JGitHelper() throws IOException {
        try {
            final URI defaultLocalGitRepo = FileSystems.getDefault().getPath(".").toUri();
            git = Git.open(new File(defaultLocalGitRepo));
            git.checkout().setName("dependencies-updates").setCreateBranch(true).call();
        } catch (Exception ex) {
            if (!(ex instanceof RefAlreadyExistsException)) {
                ex.printStackTrace();
            }
        }
    }

    /**
     * Retrieve the jGit instance
     *
     * @return the {@link Git} instance.
     * @throws IOException
     *         If an exception occurs.
     */
    public static synchronized Git git() throws IOException {
        if (INSTANCE == null) {
            INSTANCE = new JGitHelper();
        }
        return INSTANCE.git;
    }
}
