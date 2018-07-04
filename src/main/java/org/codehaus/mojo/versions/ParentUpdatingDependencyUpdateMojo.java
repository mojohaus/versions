package org.codehaus.mojo.versions;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.change.VersionChange;
import org.codehaus.mojo.versions.change.VersionChanger;
import org.codehaus.mojo.versions.change.VersionChangerFactory;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

abstract class ParentUpdatingDependencyUpdateMojo extends AbstractVersionsDependencyUpdaterMojo
{
    /**
     * Whether to process the plugins sections of the project: plugins, report plugins, and plugin management.
     *
     * @since FIXME
     */
    @Parameter( property = "processPlugins", defaultValue = "false" )
    private boolean processPlugins;

    abstract Collection<VersionChange> getVersionChanges(Collection<ArtifactIdentifier> artifacts)
            throws MojoExecutionException, ArtifactMetadataRetrievalException;

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
       throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                setDependencyVersions( pom, getProject().getDependencyManagement().getDependencies() );
            }

            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                setDependencyVersions( pom, getProject().getDependencies() );
            }

            if ( getProject().getBuildPlugins() != null && processPlugins )
            {
                setPluginVersions(pom, getProject().getBuildPlugins());
            }

            if ( getProject().getPluginManagement() != null
                    && getProject().getPluginManagement().getPlugins() != null
                    && processPlugins )
            {
                setPluginVersions(pom, getProject().getPluginManagement().getPlugins());
            }

            if (getProject().getReportPlugins() != null && processPlugins)
            {
                setReportPluginVersions(pom, getProject().getReportPlugins());
            }

            if ( getProject().getParent() != null && isProcessingParent() )
            {
                final Dependency dependency = new Dependency();
                dependency.setArtifactId(getProject().getParent().getArtifactId());
                dependency.setGroupId(getProject().getParent().getGroupId());
                dependency.setVersion(getProject().getParent().getVersion());
                dependency.setType("pom");
                setDependencyVersions( pom, Collections.singleton(dependency));
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    final void setDependencyVersions(ModifiedPomXMLEventReader pom, Iterable<Dependency> dependencies)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException
    {
        final Iterable<VersionChange> versionsToChange = getVersionChanges(dependenciesToArtifactReferences(dependencies));

        for (VersionChange versionChange : versionsToChange)
        {
            changeVersion(pom, versionChange);
        }
    }

    private void setPluginVersions(ModifiedPomXMLEventReader pom, Iterable<Plugin> plugins)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException
    {
        final Iterable<VersionChange> versionsToChange = getVersionChanges(pluginsToArtifactReferences(plugins));

        for (VersionChange versionChange : versionsToChange)
        {
            changeVersion(pom, versionChange);
        }
    }

    private void setReportPluginVersions(ModifiedPomXMLEventReader pom, Iterable<ReportPlugin> plugins)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException
    {
        final Iterable<VersionChange> versionsToChange = getVersionChanges(reportPluginsToArtifactReferences(plugins));

        for (VersionChange versionChange : versionsToChange) {
            changeVersion(pom, versionChange);
        }
    }

    private void changeVersion(ModifiedPomXMLEventReader pom, VersionChange versionChange) throws XMLStreamException
    {
        final VersionChangerFactory versionChangerFactory = new VersionChangerFactory();
        versionChangerFactory.setPom(pom);
        versionChangerFactory.setModel(getProject().getModel());
        versionChangerFactory.setLog(getLog());

        final VersionChanger versionChanger =
                versionChangerFactory.newVersionChanger(
                        isProcessingParent(),
                        false,
                        isProcessingDependencies(),
                        processPlugins);

        versionChanger.apply(versionChange);
    }

    private Collection<ArtifactIdentifier> dependenciesToArtifactReferences(Iterable<Dependency> dependencies)
    {
        // Lack of reification or functional 'map' demands this method
        final Collection<ArtifactIdentifier> ret = new ArrayList<>();

        for (Dependency dependency : dependencies)
        {
            ret.add(new DependencyArtifactIdentifier(dependency));
        }

        return ret;
    }

    private Collection<ArtifactIdentifier> pluginsToArtifactReferences(Iterable<Plugin> plugins)
    {
        // Lack of reification or functional 'map' demands this method
        final Collection<ArtifactIdentifier> ret = new ArrayList<>();

        for (Plugin plugin : plugins)
        {
            ret.add(new PluginArtifactIdentifier(plugin));
        }

        return ret;
    }

    private Collection<ArtifactIdentifier> reportPluginsToArtifactReferences(Iterable<ReportPlugin> plugins)
    {
        // Lack of reification or functional 'map' demands this method
        final Collection<ArtifactIdentifier> ret = new ArrayList<>();

        for (ReportPlugin plugin : plugins)
        {
            ret.add(new ReportPluginArtifactIdentifier(plugin));
        }

        return ret;
    }
}
