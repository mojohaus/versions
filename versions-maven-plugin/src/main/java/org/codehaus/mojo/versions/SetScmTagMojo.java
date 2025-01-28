package org.codehaus.mojo.versions;

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.maven.model.Scm;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

import static org.apache.commons.lang3.StringUtils.isAllBlank;
import static org.apache.commons.lang3.StringUtils.isBlank;

/**
 * Updates the current project's SCM tag.
 *
 * @author Anton Johansson
 * @since 2.5
 */
@Mojo(name = "set-scm-tag", aggregator = true, threadSafe = true)
public class SetScmTagMojo extends AbstractVersionsUpdaterMojo {

    /**
     * The new SCM tag to set.
     *
     * @since 2.5
     */
    @Parameter(property = "newTag")
    private String newTag;

    /**
     * The new SCM connection property
     *
     * @since 2.12.0
     */
    @Parameter(property = "connection")
    private String connection;

    /**
     * The new SCM developerConnection property
     *
     * @since 2.12.0
     */
    @Parameter(property = "developerConnection")
    private String developerConnection;

    /**
     * The new SCM url property
     *
     * @since 2.12.0
     */
    @Parameter(property = "url")
    private String url;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    @Inject
    public SetScmTagMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong.
     */
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (isAllBlank(newTag, connection, developerConnection, url)) {
            throw new MojoFailureException(
                    "One of: \"newTag\", \"connection\", \"developerConnection\", \"url\" should be provided.");
        }

        super.execute();
    }

    @Override
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        try {
            Scm scm = PomHelper.getRawModel(pom.getSource(), pom.getFileName().toFile())
                    .getScm();
            if (scm == null) {
                throw new MojoFailureException("No <scm> was present");
            }

            List<String> failures = new ArrayList<>();
            if (!isBlank(newTag)) {
                getLog().info("Updating tag: " + (scm.getTag() != null ? scm.getTag() : "(empty)") + " -> " + newTag);
                if (!PomHelper.setElementValue(pom, "/project/scm", "tag", newTag)) {
                    failures.add("tag: " + newTag);
                }
            }
            if (!isBlank(connection)) {
                getLog().info("Updating connection: "
                        + (scm.getConnection() != null ? scm.getConnection() : "(empty)") + " -> "
                        + connection);
                if (!PomHelper.setElementValue(pom, "/project/scm", "connection", connection)) {
                    failures.add("connection: " + connection);
                }
            }
            if (!isBlank(developerConnection)) {
                getLog().info("Updating developerConnection: "
                        + (scm.getDeveloperConnection() != null ? scm.getDeveloperConnection() : "(empty)")
                        + " -> "
                        + developerConnection);
                if (!PomHelper.setElementValue(pom, "/project/scm", "developerConnection", developerConnection)) {
                    failures.add("developerConnection: " + developerConnection);
                }
            }
            if (!isBlank(url)) {
                getLog().info("Updating url: " + (scm.getUrl() != null ? scm.getUrl() : "(empty)") + " -> " + url);
                if (!PomHelper.setElementValue(pom, "/project/scm", "url", url)) {
                    failures.add("url: " + url);
                }
            }
            if (!failures.isEmpty()) {
                throw new MojoFailureException("Could not update one or more SCM elements: "
                        + String.join(", ", failures) + ". Please make sure they are present in the original POM. ");
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }
}
