package org.codehaus.mojo.versions;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.mojo.versions.api.PropertyVersions;

import java.util.Collection;

/**
 * @since 1.0-beta-1
 */
public class PropertyUpdateDetails
{
    private final Property property;

    private final PropertyVersions versions;

    public PropertyUpdateDetails( Property property, PropertyVersions versions )
    {
        this.property = property;
        this.versions = versions;
    }

    public String getName()
    {
        return property.getName();
    }

    public String getVersion()
    {
        return property.getVersion();
    }

    public boolean isAutoLinkDependencies()
    {
        return property.isAutoLinkDependencies();
    }

    public Dependency[] getDependencies()
    {
        return property.getDependencies();
    }

    public boolean isSearchReactor()
    {
        return property.isSearchReactor();
    }

    public boolean isPreferReactor()
    {
        return property.isPreferReactor();
    }

    public boolean isBanSnapshots()
    {
        return property.isBanSnapshots();
    }

    public int compare( ArtifactVersion v1, ArtifactVersion v2 )
        throws MojoExecutionException
    {
        return versions.getVersionComparator().compare( v1, v2 );
    }

    /**
     * Uses the supplied {@link java.util.Collection} of {@link org.apache.maven.artifact.Artifact} instances to see if
     * an ArtifactVersion can be provided.
     *
     * @param artifacts The {@link java.util.Collection} of {@link org.apache.maven.artifact.Artifact} instances .
     * @return The versions that can be resolved from the supplied Artifact instances or an empty array if no version
     *         can be resolved (i.e. the property is not associated with any of the supplied artifacts or the property
     *         is also associated to an artifact that has not been provided).
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getVersions( Collection/*<Artifact>*/ artifacts )
        throws MojoExecutionException
    {
        return versions.getVersions( artifacts );
    }

    /**
     * Uses the {@link org.codehaus.mojo.versions.api.DefaultVersionsHelper} to find all available versions that match
     * all the associations with this property.
     *
     * @param includeSnapshots Whether to include snapshot versions in our search.
     * @return The (possibly empty) array of versions.
     */
    public ArtifactVersion[] getVersions( boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        return versions.getVersions( includeSnapshots );
    }

    public String getProfileId()
    {
        return versions.getProfileId();
    }

    public boolean isAssociated()
    {
        return versions.isAssociated();
    }

}
