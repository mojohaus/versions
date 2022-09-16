package org.codehaus.mojo.versions;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;

import static org.apache.maven.shared.utils.StringUtils.isBlank;

/**
 * Sets the parent version to the latest parent version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo( name = "update-parent",
       requiresProject = true,
       requiresDirectInvocation = true,
       threadSafe = true )
public class UpdateParentMojo extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * <p>If {@code skipResolution} is not set, specifies the <em>bottom</em> version considered
     * for target version resolution. If it is a version range, the resolved version will be
     * restricted by that range.</p>
     *
     * <p>If {@code skipResolution} is {@code true}, will specify the target version to which
     * the parent artifact will be updated.</p>
     * @since 1.0-alpha-1
     */
    @Parameter( property = "parentVersion" )
    protected String parentVersion = null;

    /**
     * to update parent version by force when it is RELEASE or LATEST
     *
     * @since 2.9
     */
    @Parameter( property = "forceUpdate", defaultValue = "false" )
    protected boolean forceUpdate = false;

    /**
     * Skips version resolution, only valid if {@code parentVersion} is set.
     * Will effectively set the new parent version to the one from {@code parentVersion}
     *
     * @since 2.13.0
     */
    @Parameter( property = "skipResolution", defaultValue = "false" )
    protected boolean skipResolution = false;

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a version within the range fulfilling the criteria.</p>
     * <p>Default <code>false</code></p>
     *
     * @since 2.12.0
     */
    @Parameter( property = "allowDowngrade",
                defaultValue = "false" )
    protected boolean allowDowngrade;

    // -------------------------- OTHER METHODS --------------------------

    @Inject
    public UpdateParentMojo( RepositorySystem repositorySystem,
                                MavenProjectBuilder projectBuilder,
                                ArtifactMetadataSource artifactMetadataSource,
                                WagonManager wagonManager,
                                ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
            throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( getProject().getParent() == null )
        {
            getLog().info( "Project does not have a parent" );
            return;
        }

        if ( reactorProjects.contains( getProject().getParent() ) )
        {
            getLog().info( "Project's parent is part of the reactor" );
            return;
        }

        if ( skipResolution && isBlank( parentVersion ) )
        {
            throw new MojoExecutionException( "skipResolution is only valid if parentVersion is set" );
        }

        String initialVersion = parentVersion == null ? getProject().getParent().getVersion() : parentVersion;
        try
        {
            ArtifactVersion artifactVersion = skipResolution ? new DefaultArtifactVersion( parentVersion )
                    : resolveTargetVersion( initialVersion );
            if ( artifactVersion != null )
            {
                getLog().info( "Updating parent from " + getProject().getParent().getVersion()
                        + " to " + artifactVersion.toString() );

                if ( PomHelper.setProjectParentVersion( pom, artifactVersion.toString() ) )
                {
                    if ( getLog().isDebugEnabled() )
                    {
                        getLog().debug( "Made an update from " + getProject().getParent().getVersion()
                                + " to " + artifactVersion );
                    }
                    getChangeRecorder().recordUpdate( "updateParent", getProject().getParent().getGroupId(),
                            getProject().getParent().getArtifactId(), getProject().getParent().getVersion(),
                            artifactVersion.toString() );
                }
            }
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( "Invalid version range specification: " + initialVersion, e );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private ArtifactVersion resolveTargetVersion( String initialVersion )
            throws MojoExecutionException, ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        Artifact artifact = getHelper().createDependencyArtifact( DependencyBuilder
                .newBuilder()
                .withGroupId( getProject().getParent().getGroupId() )
                .withArtifactId( getProject().getParent().getArtifactId() )
                .withVersion( initialVersion )
                .withType( "pom" )
                .build() );

        VersionRange targetVersionRange = VersionRange.createFromVersionSpec( initialVersion );
        if ( targetVersionRange.getRecommendedVersion() != null )
        {
            targetVersionRange = targetVersionRange.restrict(
                    VersionRange.createFromVersionSpec( "[" + targetVersionRange.getRecommendedVersion() + ",)" ) );
        }

        ArtifactVersion artifactVersion = findLatestVersion( artifact, targetVersionRange, null,
                false, allowDowngrade );
        if ( !shouldApplyUpdate( artifact, getProject().getParent().getVersion(), artifactVersion, forceUpdate ) )
        {
            getLog().debug( "Update not applied. Exiting." );
            return null;
        }
        return artifactVersion;
    }

}
