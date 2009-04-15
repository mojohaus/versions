package org.codehaus.mojo.versions.api;

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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.settings.Settings;
import org.apache.maven.wagon.ConnectionException;
import org.apache.maven.wagon.ResourceDoesNotExistException;
import org.apache.maven.wagon.TransferFailedException;
import org.apache.maven.wagon.UnsupportedProtocolException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationException;
import org.apache.maven.wagon.authorization.AuthorizationException;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.model.io.xpp3.RuleXpp3Reader;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.mojo.versions.utils.WagonUtils;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public class DefaultVersionsHelper
    implements VersionsHelper
{
    /**
     * The artifact comparison rules to use.
     *
     * @since 1.0-alpha-3
     */
    private final RuleSet ruleSet;

    /**
     * The artifact metadata source to use.
     *
     * @since 1.0-alpha-3
     */
    private final ArtifactMetadataSource artifactMetadataSource;

    /**
     * The local repository to consult.
     *
     * @since 1.0-alpha-3
     */
    private final ArtifactRepository localRepository;

    /**
     * The remote artifact repositories to consult.
     *
     * @since 1.0-alpha-3
     */
    private final List remoteArtifactRepositories;

    /**
     * The remote plugin repositories to consult.
     *
     * @since 1.0-alpha-3
     */
    private final List remotePluginRepositories;

    /**
     * The artifact factory.
     *
     * @since 1.0-alpha-3
     */
    private final ArtifactFactory artifactFactory;

    /**
     * The {@link Log} to send log messages to.
     *
     * @since 1.0-alpha-3
     */
    private final Log log;

    /**
     * Constructs a new {@link DefaultVersionsHelper}.
     *
     * @param artifactFactory            The artifact factory.
     * @param artifactMetadataSource     The artifact metadata source to use.
     * @param remoteArtifactRepositories The remote artifact repositories to consult.
     * @param remotePluginRepositories   The remote plugin repositories to consult.
     * @param localRepository            The local repository to consult.
     * @param wagonManager               The wagon manager (used if rules need to be retrieved).
     * @param settings                   The settings  (used to provide proxy information to the wagon manager).
     * @param serverId                   The serverId hint for the wagon manager.
     * @param rulesUri                   The URL to retrieve the versioning rules from.
     * @param comparisonMethod           The default comparison method.
     * @param log                        The {@link org.apache.maven.plugin.logging.Log} to send log messages to. @since 1.0-alpha-3
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          If things go wrong.
     */
    public DefaultVersionsHelper( ArtifactFactory artifactFactory, ArtifactMetadataSource artifactMetadataSource,
                                  List remoteArtifactRepositories, List remotePluginRepositories,
                                  ArtifactRepository localRepository, WagonManager wagonManager, Settings settings,
                                  String serverId, String rulesUri, String comparisonMethod, Log log )
        throws MojoExecutionException
    {
        this.artifactFactory = artifactFactory;
        this.ruleSet = loadRuleSet( serverId, settings, wagonManager, rulesUri, comparisonMethod, log );
        this.artifactMetadataSource = artifactMetadataSource;
        this.localRepository = localRepository;
        this.remoteArtifactRepositories = remoteArtifactRepositories;
        this.remotePluginRepositories = remotePluginRepositories;
        this.log = log;
    }

    public ArtifactFactory getArtifactFactory()
    {
        return artifactFactory;
    }

    public Log getLog()
    {
        return log;
    }

    public ArtifactVersions lookupArtifactVersions( Artifact artifact, boolean usePluginRepositories )
        throws MojoExecutionException
    {
        return lookupVersions( artifact,
                               usePluginRepositories ? remotePluginRepositories : remoteArtifactRepositories );
    }

    /**
     * Looks up the versions of the specified artifact that are available in either the local repository or the
     * specified remote repositories.
     *
     * @param artifact           The artifact to lookup.
     * @param remoteRepositories The remote repositories to consult.
     * @return The ArtifactVersions with details of the available versions.
     * @throws MojoExecutionException When things go wrong.
     * @since 1.0-alpha-3
     */
    private ArtifactVersions lookupVersions( Artifact artifact, List remoteRepositories )
        throws MojoExecutionException
    {
        final List versions;
        try
        {
            versions =
                artifactMetadataSource.retrieveAvailableVersions( artifact, localRepository, remoteRepositories );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( "Could not retrieve metadata for " + artifact, e );
        }
        return new ArtifactVersions( artifact, versions, getVersionComparator( artifact ) );
    }


    public Comparator getVersionComparator( Artifact artifact )
    {
        return getVersionComparator( artifact.getGroupId(), artifact.getArtifactId() );
    }

    public Comparator getVersionComparator( String groupId, String artifactId )
    {
        final List/*<Rule>*/ rules = ruleSet.getRules();
        String comparisonMethod = ruleSet.getComparisonMethod();
        int bestGroupIdScore = Integer.MAX_VALUE;
        int bestArtifactIdScore = Integer.MAX_VALUE;
        boolean exactGroupId = false;
        boolean exactArtifactId = false;
        for ( Iterator i = rules.iterator(); i.hasNext(); )
        {
            Rule rule = (Rule) i.next();
            int groupIdScore = RegexUtils.getWildcardScore( rule.getGroupId() );
            if ( groupIdScore > bestGroupIdScore )
            {
                continue;
            }
            boolean exactMatch = exactMatch( rule.getGroupId(), groupId );
            boolean match = exactMatch || match( rule.getGroupId(), groupId );
            if ( !match || ( exactGroupId && !exactMatch ) )
            {
                continue;
            }
            if ( bestGroupIdScore > groupIdScore )
            {
                bestArtifactIdScore = Integer.MAX_VALUE;
                exactArtifactId = false;
            }
            bestGroupIdScore = groupIdScore;
            if ( exactMatch && !exactGroupId )
            {
                exactGroupId = true;
                bestArtifactIdScore = Integer.MAX_VALUE;
                exactArtifactId = false;
            }
            int artifactIdScore = RegexUtils.getWildcardScore( rule.getArtifactId() );
            if ( artifactIdScore > bestArtifactIdScore )
            {
                continue;
            }
            exactMatch = exactMatch( rule.getArtifactId(), artifactId );
            match = exactMatch || match( rule.getArtifactId(), artifactId );
            if ( !match || ( exactArtifactId && !exactMatch ) )
            {
                continue;
            }
            bestArtifactIdScore = artifactIdScore;
            if ( exactMatch && !exactArtifactId )
            {
                exactArtifactId = true;
            }
            comparisonMethod = rule.getComparisonMethod();
        }
        return VersionComparators.getVersionComparator( comparisonMethod );
    }


    private static RuleSet getRuleSet( Wagon wagon, String remoteURI )
        throws IOException, AuthorizationException, TransferFailedException, ResourceDoesNotExistException
    {
        File tempFile = File.createTempFile( "ruleset", ".xml" );
        try
        {
            wagon.get( remoteURI, tempFile );
            RuleXpp3Reader reader = new RuleXpp3Reader();
            FileInputStream fis = new FileInputStream( tempFile );
            try
            {
                BufferedInputStream bis = new BufferedInputStream( fis );
                try
                {
                    return reader.read( bis );
                }
                catch ( XmlPullParserException e )
                {
                    final IOException ioe = new IOException();
                    ioe.initCause( e );
                    throw ioe;
                }
                finally
                {
                    try
                    {
                        bis.close();
                    }
                    catch ( IOException e )
                    {
                        // ignore
                    }
                }
            }
            finally
            {
                try
                {
                    fis.close();
                }
                catch ( IOException e )
                {
                    // ignore
                }
            }
        }
        finally
        {
            tempFile.delete();
        }
    }

    static boolean exactMatch( String wildcardRule, String value )
    {
        Pattern p = Pattern.compile( RegexUtils.convertWildcardsToRegex( wildcardRule, true ) );
        return p.matcher( value ).matches();
    }

    static boolean match( String wildcardRule, String value )
    {
        Pattern p = Pattern.compile( RegexUtils.convertWildcardsToRegex( wildcardRule, false ) );
        return p.matcher( value ).matches();
    }

    private static RuleSet loadRuleSet( String serverId, Settings settings, WagonManager wagonManager, String rulesUri,
                                        String comparisonMethod, Log logger )
        throws MojoExecutionException
    {
        RuleSet ruleSet = new RuleSet();
        if ( comparisonMethod != null )
        {
            ruleSet.setComparisonMethod( comparisonMethod );
        }
        if ( rulesUri != null && rulesUri.trim().length() != 0 )
        {
            try
            {

                try
                {
                    Wagon wagon = WagonUtils.createWagon( serverId, rulesUri, wagonManager, settings, logger );
                    try
                    {
                        ruleSet.setRules( getRuleSet( wagon, "" ).getRules() );
                    }
                    finally
                    {
                        if ( wagon != null )
                        {
                            try
                            {
                                wagon.disconnect();
                            }
                            catch ( ConnectionException e )
                            {
                                logger.warn( "Could not disconnect wagon!", e );
                            }
                        }

                    }
                }
                catch ( TransferFailedException e )
                {
                    throw new MojoExecutionException( "Could not transfer rules from " + rulesUri, e );
                }
                catch ( AuthorizationException e )
                {
                    throw new MojoExecutionException( "Authorization failure trying to load rules from " + rulesUri,
                                                      e );
                }
                catch ( ResourceDoesNotExistException e )
                {
                    throw new MojoExecutionException( "Could not load specified rules from " + rulesUri, e );
                }
                catch ( AuthenticationException e )
                {
                    throw new MojoExecutionException( "Authentication failure trying to load rules from " + rulesUri,
                                                      e );
                }
                catch ( UnsupportedProtocolException e )
                {
                    throw new MojoExecutionException( "Unsupported protocol for " + rulesUri, e );
                }
                catch ( ConnectionException e )
                {
                    throw new MojoExecutionException( "Could not establish connection to " + rulesUri, e );
                }
            }
            catch ( IOException e )
            {
                throw new MojoExecutionException( "Could not load specified rules from " + rulesUri, e );
            }
        }
        return ruleSet;
    }

    public Artifact createPluginArtifact( String groupId, String artifactId, VersionRange versionRange )
    {
        return artifactFactory.createPluginArtifact( groupId, artifactId, versionRange );
    }

    public Artifact createDependencyArtifact( String groupId, String artifactId, VersionRange versionRange, String type,
                                              String classifier, String scope, boolean optional )
    {
        return artifactFactory.createDependencyArtifact( groupId, artifactId, versionRange, type, classifier, scope,
                                                         optional );
    }

    public Artifact createDependencyArtifact( Dependency dependency )
        throws InvalidVersionSpecificationException
    {
        return createDependencyArtifact( dependency.getGroupId(), dependency.getArtifactId(),
                                         dependency.getVersion() == null
                                             ? VersionRange.createFromVersionSpec( "[0,]" )
                                             : VersionRange.createFromVersionSpec( dependency.getVersion() ),
                                         dependency.getType(), dependency.getClassifier(), dependency.getScope(),
                                         dependency.isOptional() );
    }

    /**
     * Takes a {@link java.util.List} of {@link org.apache.maven.project.MavenProject} instances and converts it into a {@link java.util.Set} of {@link org.apache.maven.artifact.Artifact} instances.
     *
     * @param mavenProjects the {@link java.util.List} of {@link org.apache.maven.project.MavenProject} instances.
     * @return a {@link java.util.Set} of {@link org.apache.maven.artifact.Artifact} instances.
     * @throws org.apache.maven.artifact.versioning.InvalidVersionSpecificationException
     *          if any of the {@link org.apache.maven.project.MavenProject} versions are invalid (should never happen).
     * @since 1.0-alpha-3
     */
    public Set/*<Artifact>*/ extractArtifacts( Collection/*<MavenProject>*/ mavenProjects )
    {
        Set/*<Artifact>*/ result = new HashSet();
        Iterator i = mavenProjects.iterator();
        while ( i.hasNext() )
        {
            Object next = i.next();
            if ( next instanceof MavenProject )
            {
                MavenProject project = (MavenProject) next;
                result.add( project.getArtifact() );
            }
        }

        return result;
    }

    /**
     * Returns <code>true</code> if the artifact matches the include/exclude rules. Include superceeds exclude.
     *
     * @param artifact           The artifact to query
     * @param includeGroupIds    A comma separated list of group Ids to include, or <code>null</code> or an empty string to include any.
     * @param includeArtifactIds A comma separated list of artifact Ids to include, or <code>null</code> or an empty string to include any.
     * @param excludeGroupIds    A comma separated list of group Ids to exclude, or <code>null</code> or an empty string to exclude none.
     * @param excludeArtifactIds A comma separated list of artifact Ids to exclude, or <code>null</code> or an empty string to exclude none.
     * @return <code>true</code> if the artifact can be included.
     */
    public boolean isIncluded( Artifact artifact, String includeGroupIds, String includeArtifactIds,
                               String excludeGroupIds, String excludeArtifactIds )
    {
        Pattern groupIdPattern =
            Pattern.compile( "(.*,)?\\s*" + RegexUtils.quote( artifact.getGroupId() ) + "\\s*(,.*)?" );
        boolean haveIncludeGroupIds = includeGroupIds != null && includeGroupIds.trim().length() != 0;
        boolean haveIncludeGroupIdsMatch = haveIncludeGroupIds && groupIdPattern.matcher( includeGroupIds ).matches();
        boolean haveExcludeGroupIds = excludeGroupIds != null && excludeGroupIds.trim().length() != 0;
        boolean haveExcludeGroupIdsMatch = haveExcludeGroupIds && groupIdPattern.matcher( excludeGroupIds ).matches();
        Pattern artifactIdPattern =
            Pattern.compile( "(.*,)?\\s*" + RegexUtils.quote( artifact.getArtifactId() ) + "\\s*(,.*)?" );
        boolean haveIncludeArtifactIds = includeArtifactIds != null && includeArtifactIds.trim().length() != 0;
        boolean haveIncludeArtifactIdsMatch =
            haveIncludeArtifactIds && artifactIdPattern.matcher( includeArtifactIds ).matches();
        boolean haveExcludeArtifactIds = excludeArtifactIds != null && excludeArtifactIds.trim().length() != 0;
        boolean haveExcludeArtifactIdsMatch =
            haveExcludeArtifactIds && artifactIdPattern.matcher( excludeArtifactIds ).matches();

        return ( !haveIncludeGroupIds || haveIncludeGroupIdsMatch )
            && ( !haveExcludeGroupIds || !haveExcludeGroupIdsMatch || haveIncludeGroupIdsMatch )
            && ( !haveIncludeArtifactIds || haveIncludeArtifactIdsMatch ) && ( !haveExcludeArtifactIds
            || !haveExcludeArtifactIdsMatch || haveIncludeArtifactIdsMatch );
    }
}
