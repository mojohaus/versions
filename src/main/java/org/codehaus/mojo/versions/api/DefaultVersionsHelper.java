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
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.path.PathTranslator;
import org.apache.maven.settings.Settings;
import org.apache.maven.wagon.ConnectionException;
import org.apache.maven.wagon.ResourceDoesNotExistException;
import org.apache.maven.wagon.TransferFailedException;
import org.apache.maven.wagon.UnsupportedProtocolException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationException;
import org.apache.maven.wagon.authorization.AuthorizationException;
import org.codehaus.mojo.versions.ArtifactUpdatesDetails;
import org.codehaus.mojo.versions.PluginUpdatesDetails;
import org.codehaus.mojo.versions.Property;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.model.io.xpp3.RuleXpp3Reader;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.mojo.versions.utils.VersionsExpressionEvaluator;
import org.codehaus.mojo.versions.utils.WagonUtils;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
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
     * The path translator.
     *
     * @since 1.0-beta-1
     */
    private final PathTranslator pathTranslator;

    /**
     * The maven session.
     *
     * @since 1.0-beta-1
     */
    private final MavenSession mavenSession;

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
     * @param mavenSession               The maven session information.
     * @param pathTranslator             The path translator component.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          If things go wrong.
     */
    public DefaultVersionsHelper( ArtifactFactory artifactFactory, ArtifactMetadataSource artifactMetadataSource,
                                  List remoteArtifactRepositories, List remotePluginRepositories,
                                  ArtifactRepository localRepository, WagonManager wagonManager, Settings settings,
                                  String serverId, String rulesUri, String comparisonMethod, Log log,
                                  MavenSession mavenSession, PathTranslator pathTranslator )
        throws MojoExecutionException
    {
        this.artifactFactory = artifactFactory;
        this.mavenSession = mavenSession;
        this.pathTranslator = pathTranslator;
        this.ruleSet = loadRuleSet( serverId, settings, wagonManager, rulesUri, comparisonMethod, log );
        this.artifactMetadataSource = artifactMetadataSource;
        this.localRepository = localRepository;
        this.remoteArtifactRepositories = remoteArtifactRepositories;
        this.remotePluginRepositories = remotePluginRepositories;
        this.log = log;
    }

    /**
     * {@inheritDoc}
     */
    public ArtifactFactory getArtifactFactory()
    {
        return artifactFactory;
    }

    /**
     * {@inheritDoc}
     */
    public Log getLog()
    {
        return log;
    }

    /**
     * {@inheritDoc}
     */
    public ArtifactVersions lookupArtifactVersions( Artifact artifact, boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException
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
     * @throws ArtifactMetadataRetrievalException
     *          When things go wrong.
     * @since 1.0-alpha-3
     */
    private ArtifactVersions lookupVersions( Artifact artifact, List remoteRepositories )
        throws ArtifactMetadataRetrievalException
    {
        final List versions;
        versions = artifactMetadataSource.retrieveAvailableVersions( artifact, localRepository, remoteRepositories );
        return new ArtifactVersions( artifact, versions, getVersionComparator( artifact ) );
    }

    /**
     * {@inheritDoc}
     */
    public VersionComparator getVersionComparator( Artifact artifact )
    {
        return getVersionComparator( artifact.getGroupId(), artifact.getArtifactId() );
    }

    /**
     * {@inheritDoc}
     */
    public VersionComparator getVersionComparator( String groupId, String artifactId )
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
            if ( !tempFile.delete() )
            {
                // maybe we can delete this later
                tempFile.deleteOnExit();
            }
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
                int split = rulesUri.lastIndexOf( '/' );
                String baseUri;
                String fileUri;
                if ( split != -1 )
                {
                    baseUri = rulesUri.substring( 0, split ) + '/';
                    fileUri = split + 1 < rulesUri.length() ? rulesUri.substring( split + 1 ) : "";
                }
                else
                {
                    baseUri = rulesUri;
                    fileUri = "";
                }
                try
                {
                    Wagon wagon = WagonUtils.createWagon( serverId, baseUri, wagonManager, settings, logger );
                    try
                    {
                        logger.debug( "Trying to load ruleset from file \"" + fileUri + "\" in " + baseUri );
                        ruleSet.setRules( getRuleSet( wagon, fileUri ).getRules() );
                        logger.debug( "Rule set loaded" );
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

    /**
     * {@inheritDoc}
     */
    public Artifact createPluginArtifact( String groupId, String artifactId, VersionRange versionRange )
    {
        return artifactFactory.createPluginArtifact( groupId, artifactId, versionRange );
    }

    /**
     * {@inheritDoc}
     */
    public Artifact createDependencyArtifact( String groupId, String artifactId, VersionRange versionRange, String type,
                                              String classifier, String scope, boolean optional )
    {
        return artifactFactory.createDependencyArtifact( groupId, artifactId, versionRange, type, classifier, scope,
                                                         optional );
    }

    /**
     * {@inheritDoc}
     */
    public Artifact createDependencyArtifact( String groupId, String artifactId, VersionRange versionRange, String type,
                                              String classifier, String scope )
    {
        return artifactFactory.createDependencyArtifact( groupId, artifactId, versionRange, type, classifier, scope );
    }

    /**
     * {@inheritDoc}
     */
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
     * {@inheritDoc}
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
     * {@inheritDoc}
     */
    public ArtifactVersion createArtifactVersion( String version )
    {
        return new DefaultArtifactVersion( version );
    }

    /**
     * {@inheritDoc}
     */
    public ArtifactUpdatesDetails lookupArtifactUpdates( Artifact artifact, ArtifactVersion current,
                                                         Boolean allowSnapshots, boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException
    {
        ArtifactVersions artifactVersions = lookupArtifactVersions( artifact, usePluginRepositories );

        VersionComparator versionComparator = artifactVersions.getVersionComparator();
        int segmentCount = versionComparator.getSegmentCount( current );
        ArtifactVersion nextVersion = segmentCount < 3
            ? null
            : artifactVersions.getOldestVersion( current, versionComparator.incrementSegment( current, 2 ),
                                                 Boolean.TRUE.equals( allowSnapshots ), false, false );
        ArtifactVersion nextIncremental = segmentCount < 3
            ? null
            : artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 2 ),
                                                 versionComparator.incrementSegment( current, 1 ),
                                                 Boolean.TRUE.equals( allowSnapshots ), true, false );
        ArtifactVersion latestIncremental = segmentCount < 3
            ? null
            : artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 2 ),
                                                 versionComparator.incrementSegment( current, 1 ),
                                                 Boolean.TRUE.equals( allowSnapshots ), true, false );
        ArtifactVersion nextMinor = segmentCount < 2
            ? null
            : artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 1 ),
                                                 versionComparator.incrementSegment( current, 0 ),
                                                 Boolean.TRUE.equals( allowSnapshots ), true, false );
        ArtifactVersion latestMinor = segmentCount < 2
            ? null
            : artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 1 ),
                                                 versionComparator.incrementSegment( current, 0 ),
                                                 Boolean.TRUE.equals( allowSnapshots ), true, false );
        ArtifactVersion nextMajor =
            artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                               Boolean.TRUE.equals( allowSnapshots ), true, false );
        ArtifactVersion latestMajor =
            artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                               Boolean.TRUE.equals( allowSnapshots ), true, false );

        return new ArtifactUpdatesDetails( artifact, nextVersion, nextIncremental, latestIncremental, nextMinor,
                                           latestMinor, nextMajor, latestMajor,
                                           artifactVersions.getNewerVersions( current ) );
    }

    /**
     * {@inheritDoc}
     */
    public Map/*<Dependency,ArtifactUpdatesDetails>*/ lookupDependenciesUpdates( Set dependencies,
                                                                                 Boolean allowSnapshots,
                                                                                 boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        Map/*<Dependency,ArtifactUpdatesDetails>*/ dependencyUpdates = new TreeMap( new DependencyComparator() );
        Iterator i = dependencies.iterator();
        while ( i.hasNext() )
        {
            Dependency dependency = (Dependency) i.next();

            ArtifactUpdatesDetails details =
                lookupDependencyUpdates( dependency, allowSnapshots, usePluginRepositories );
            dependencyUpdates.put( dependency, details );
        }
        return dependencyUpdates;
    }

    /**
     * {@inheritDoc}
     */
    public ArtifactUpdatesDetails lookupDependencyUpdates( Dependency dependency, Boolean allowSnapshots,
                                                           boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        getLog().debug(
            "Checking " + ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() )
                + " for updates newer than " + dependency.getVersion() );
        VersionRange versionRange = VersionRange.createFromVersionSpec( dependency.getVersion() );

        return lookupArtifactUpdates(
            createDependencyArtifact( dependency.getGroupId(), dependency.getArtifactId(), versionRange,
                                      dependency.getType(), dependency.getClassifier(), dependency.getScope() ),
            createArtifactVersion( dependency.getVersion() ), allowSnapshots, usePluginRepositories );
    }

    /**
     * {@inheritDoc}
     */
    public Map/*<Plugin,PluginUpdateDetails>*/ lookupPluginsUpdates( Set plugins, Boolean allowSnapshots )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        Map/*<Plugin,PluginUpdateDetails>*/ pluginUpdates = new TreeMap( new PluginComparator() );
        Iterator i = plugins.iterator();
        while ( i.hasNext() )
        {
            Plugin plugin = (Plugin) i.next();
            PluginUpdatesDetails details = lookupPluginUpdates( plugin, allowSnapshots );
            pluginUpdates.put( plugin, details );
        }
        return pluginUpdates;
    }

    /**
     * {@inheritDoc}
     */
    public PluginUpdatesDetails lookupPluginUpdates( Plugin plugin, Boolean allowSnapshots )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        getLog().debug( "Checking " + ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() )
            + " for updates newer than " + plugin.getVersion() );

        VersionRange versionRange = VersionRange.createFromVersion( plugin.getVersion() );

        final ArtifactUpdatesDetails pluginArtifactDetails =
            lookupArtifactUpdates( createPluginArtifact( plugin.getGroupId(), plugin.getArtifactId(), versionRange ),
                                   createArtifactVersion( plugin.getVersion() ), allowSnapshots, true );

        Set/*<Dependency>*/ pluginDependencies = new TreeSet( new DependencyComparator() );
        if ( plugin.getDependencies() != null )
        {
            pluginDependencies.addAll( plugin.getDependencies() );
        }
        Map/*<Dependency,ArtifactUpdatesDetails>*/ pluginDependencyDetails =
            lookupDependenciesUpdates( pluginDependencies, allowSnapshots, false );

        return new PluginUpdatesDetails( pluginArtifactDetails, pluginDependencyDetails );
    }

    /**
     * {@inheritDoc}
     */
    public ExpressionEvaluator getExpressionEvaluator( MavenProject project )
    {
        return new VersionsExpressionEvaluator( mavenSession, pathTranslator, project );
    }

    /**
     * {@inheritDoc}
     */
    public Map/*<Property,PropertyVersions>*/ getVersionPropertiesMap( MavenProject project,
                                                                       Property[] propertyDefinitions,
                                                                       String includeProperties,
                                                                       String excludeProperties, boolean autoLinkItems )
        throws MojoExecutionException
    {
        Map properties = new HashMap();
        if ( propertyDefinitions != null )
        {
            for ( int i = 0; i < propertyDefinitions.length; i++ )
            {
                properties.put( propertyDefinitions[i].getName(), propertyDefinitions[i] );
            }
        }
        Map versions = new HashMap();
        if ( autoLinkItems )
        {
            final PropertyVersions[] propertyVersions;
            try
            {
                propertyVersions = PomHelper.getPropertyVersions( this, project );
            }
            catch ( ExpressionEvaluationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
            catch ( IOException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }

            for ( int i = 0; i < propertyVersions.length; i++ )
            {
                versions.put( propertyVersions[i].getName(), propertyVersions[i] );
                if ( !properties.containsKey( propertyVersions[i].getName() ) )
                {
                    properties.put( propertyVersions[i].getName(), new Property( propertyVersions[i] ) );
                }
            }
        }
        getLog().info( "Searching for properties to update" );
        Iterator i = properties.values().iterator();
        while ( i.hasNext() )
        {
            Property property = (Property) i.next();
            if ( includeProperties != null && includeProperties.indexOf( property.getName() ) < 0 )
            {
                getLog().debug( "Skipping update of property ${" + property.getName() + "}" );
                i.remove();
            }

            if ( excludeProperties != null && excludeProperties.indexOf( property.getName() ) >= 0 )
            {
                getLog().debug( "Ignoring update of property ${" + property.getName() + "}" );
                i.remove();
            }
        }
        i = properties.values().iterator();
        Map/*<Property,PropertyVersions>*/ propertyVersions = new LinkedHashMap( properties.size() );
        while ( i.hasNext() )
        {
            Property property = (Property) i.next();
            getLog().debug( "Property ${" + property.getName() + "}" );
            PropertyVersions version = (PropertyVersions) versions.get( property.getName() );
            if ( version == null || !version.isAssociated() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Looks like this property is not "
                    + "associated with any dependency..." );
                version = new PropertyVersions( null, property.getName(), this );
            }
            if ( !property.isAutoLinkDependencies() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Removing any autoLinkDependencies" );
                version.clearAssociations();
            }
            Dependency[] dependencies = property.getDependencies();
            if ( dependencies != null )
            {
                for ( int j = 0; j < dependencies.length; j++ )
                {
                    try
                    {
                        getLog().debug(
                            "Property ${" + property.getName() + "}: Adding association to " + dependencies[j] );
                        version.addAssociation( this.createDependencyArtifact( dependencies[j] ), false );
                    }
                    catch ( InvalidVersionSpecificationException e )
                    {
                        throw new MojoExecutionException( e.getMessage(), e );
                    }
                }
            }
            propertyVersions.put( property, version );
        }
        return propertyVersions;
    }


}
