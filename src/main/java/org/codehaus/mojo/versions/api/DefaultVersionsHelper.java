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
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolver;
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
import org.codehaus.mojo.versions.PluginUpdatesDetails;
import org.codehaus.mojo.versions.Property;
import org.codehaus.mojo.versions.model.IgnoreVersion;
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
import org.codehaus.plexus.util.StringUtils;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
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
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Pattern;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class DefaultVersionsHelper
    implements VersionsHelper
{
    private static final String TYPE_EXACT = "exact";

    private static final String TYPE_REGEX = "regex";

    private static final int LOOKUP_PARALLEL_THREADS = 5;

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
     * The artifact resolver.
     *
     * @since 1.3
     */
    private final ArtifactResolver artifactResolver;

    /**
     * Constructs a new {@link DefaultVersionsHelper}.
     *
     * @param artifactFactory The artifact factory.
     * @param artifactResolver
     * @param artifactMetadataSource The artifact metadata source to use.
     * @param remoteArtifactRepositories The remote artifact repositories to consult.
     * @param remotePluginRepositories The remote plugin repositories to consult.
     * @param localRepository The local repository to consult.
     * @param wagonManager The wagon manager (used if rules need to be retrieved).
     * @param settings The settings (used to provide proxy information to the wagon manager).
     * @param serverId The serverId hint for the wagon manager.
     * @param rulesUri The URL to retrieve the versioning rules from.
     * @param log The {@link org.apache.maven.plugin.logging.Log} to send log messages to.
     * @param mavenSession The maven session information.
     * @param pathTranslator The path translator component. @throws org.apache.maven.plugin.MojoExecutionException If
     *            things go wrong.
     * @since 1.0-alpha-3
     */
    public DefaultVersionsHelper( ArtifactFactory artifactFactory, ArtifactResolver artifactResolver,
                                  ArtifactMetadataSource artifactMetadataSource, List remoteArtifactRepositories,
                                  List remotePluginRepositories, ArtifactRepository localRepository,
                                  WagonManager wagonManager, Settings settings, String serverId, String rulesUri,
                                  Log log, MavenSession mavenSession, PathTranslator pathTranslator )
                                      throws MojoExecutionException
    {
        this.artifactFactory = artifactFactory;
        this.artifactResolver = artifactResolver;
        this.mavenSession = mavenSession;
        this.pathTranslator = pathTranslator;
        this.ruleSet = loadRuleSet( serverId, settings, wagonManager, rulesUri, log );
        this.artifactMetadataSource = artifactMetadataSource;
        this.localRepository = localRepository;
        this.remoteArtifactRepositories = remoteArtifactRepositories;
        this.remotePluginRepositories = remotePluginRepositories;
        this.log = log;
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
                                        Log logger )
                                            throws MojoExecutionException
    {
        RuleSet ruleSet = new RuleSet();
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
                        final RuleSet loaded = getRuleSet( wagon, fileUri );
                        ruleSet.setRules( loaded.getRules() );
                        ruleSet.setIgnoreVersions( loaded.getIgnoreVersions() );
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
        List remoteRepositories = usePluginRepositories ? remotePluginRepositories : remoteArtifactRepositories;
        final List<ArtifactVersion> versions =
            artifactMetadataSource.retrieveAvailableVersions( artifact, localRepository, remoteRepositories );
        final List<IgnoreVersion> ignoredVersions = getIgnoredVersions( artifact );
        if ( !ignoredVersions.isEmpty() )
        {
            if ( getLog().isDebugEnabled() )
            {
                getLog().debug( "Found ignored versions: " + showIgnoredVersions( ignoredVersions ) );
            }

            final Iterator<ArtifactVersion> i = versions.iterator();
            while ( i.hasNext() )
            {
                final String version = i.next().toString();
                for ( final IgnoreVersion ignoreVersion : ignoredVersions )
                {
                    if ( TYPE_REGEX.equals( ignoreVersion.getType() ) )
                    {
                        Pattern p = Pattern.compile( ignoreVersion.getVersion() );
                        if ( p.matcher( version ).matches() )
                        {
                            if ( getLog().isDebugEnabled() )
                            {
                                getLog().debug( "Version " + version + " for artifact "
                                    + ArtifactUtils.versionlessKey( artifact ) + " found on ignore list: "
                                    + ignoreVersion );
                            }
                            i.remove();
                            break;
                        }
                    }
                    else if ( TYPE_EXACT.equals( ignoreVersion.getType() ) )
                    {
                        if ( version.equals( ignoreVersion.getVersion() ) )
                        {
                            if ( getLog().isDebugEnabled() )
                            {
                                getLog().debug( "Version " + version + " for artifact "
                                    + ArtifactUtils.versionlessKey( artifact ) + " found on ignore list: "
                                    + ignoreVersion );
                            }
                            i.remove();
                            break;
                        }
                    }
                }
            }
        }
        return new ArtifactVersions( artifact, versions, getVersionComparator( artifact ) );
    }

    /**
     * Returns a list of versions which should not be considered when looking for updates.
     *
     * @param artifact The artifact
     * @return List of ignored version
     */
    private List<IgnoreVersion> getIgnoredVersions( Artifact artifact )
    {
        final List<IgnoreVersion> ret = new ArrayList<IgnoreVersion>();

        for ( final IgnoreVersion ignoreVersion : ruleSet.getIgnoreVersions() )
        {
            if ( !TYPE_EXACT.equals( ignoreVersion.getType() ) && !TYPE_REGEX.equals( ignoreVersion.getType() ) )
            {
                getLog().warn( "The type attribute '" + ignoreVersion.getType() + "' for global ignoreVersion["
                    + ignoreVersion + "] is not valid." + " Please use either '" + TYPE_EXACT + "' or '" + TYPE_REGEX
                    + "'." );
            }
            else
            {
                ret.add( ignoreVersion );
            }
        }

        final Rule rule = getBestFitRule( artifact.getGroupId(), artifact.getArtifactId() );

        if ( rule != null )
        {
            for ( IgnoreVersion ignoreVersion : rule.getIgnoreVersions() )
            {
                if ( !TYPE_EXACT.equals( ignoreVersion.getType() ) && !TYPE_REGEX.equals( ignoreVersion.getType() ) )
                {
                    getLog().warn( "The type attribute '" + ignoreVersion.getType() + "' for " + rule + " is not valid."
                        + " Please use either '" + TYPE_EXACT + "' or '" + TYPE_REGEX + "'." );
                }
                else
                {
                    ret.add( ignoreVersion );
                }
            }
        }

        return ret;
    }

    /**
     * Pretty print a list of ignored versions.
     *
     * @param ignoredVersions A list of ignored versions
     * @return A String representation of the list
     */
    private String showIgnoredVersions( List<IgnoreVersion> ignoredVersions )
    {
        StringBuilder buf = new StringBuilder();
        Iterator<IgnoreVersion> iterator = ignoredVersions.iterator();
        while ( iterator.hasNext() )
        {
            IgnoreVersion ignoreVersion = iterator.next();
            buf.append( ignoreVersion );
            if ( iterator.hasNext() )
            {
                buf.append( ", " );
            }
        }
        return buf.toString();
    }

    public void resolveArtifact( Artifact artifact, boolean usePluginRepositories )
        throws ArtifactResolutionException, ArtifactNotFoundException
    {
        List remoteRepositories = usePluginRepositories ? remotePluginRepositories : remoteArtifactRepositories;
        artifactResolver.resolve( artifact, remoteRepositories, localRepository );
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
        Rule rule = getBestFitRule( groupId, artifactId );
        final String comparisonMethod = rule == null ? ruleSet.getComparisonMethod() : rule.getComparisonMethod();
        return VersionComparators.getVersionComparator( comparisonMethod );
    }

    /**
     * Find the rule, if any, which best fits the artifact details given.
     *
     * @param groupId Group id of the artifact
     * @param artifactId Artifact id of the artifact
     * @return Rule which best describes the given artifact
     */
    protected Rule getBestFitRule( String groupId, String artifactId )
    {
        Rule bestFit = null;
        final List<Rule> rules = ruleSet.getRules();
        int bestGroupIdScore = Integer.MAX_VALUE;
        int bestArtifactIdScore = Integer.MAX_VALUE;
        boolean exactGroupId = false;
        boolean exactArtifactId = false;
        for ( Rule rule : rules )
        {
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
            bestFit = rule;
        }
        return bestFit;
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
                                         dependency.getVersion() == null ? VersionRange.createFromVersionSpec( "[0,]" )
                                                         : VersionRange.createFromVersionSpec( dependency.getVersion() ),
                                         dependency.getType(), dependency.getClassifier(), dependency.getScope(),
                                         dependency.isOptional() );
    }

    /**
     * {@inheritDoc}
     */
    public Set<Artifact> extractArtifacts( Collection<MavenProject> mavenProjects )
    {
        Set<Artifact> result = new HashSet<Artifact>();
        for ( MavenProject project : mavenProjects )
        {
            result.add( project.getArtifact() );
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
    public ArtifactVersions lookupArtifactUpdates( Artifact artifact, Boolean allowSnapshots,
                                                   boolean usePluginRepositories )
                                                       throws ArtifactMetadataRetrievalException
    {
        ArtifactVersions artifactVersions = lookupArtifactVersions( artifact, usePluginRepositories );

        artifactVersions.setIncludeSnapshots( Boolean.TRUE.equals( allowSnapshots ) );

        return artifactVersions;
    }

    /**
     * {@inheritDoc}
     */
    public Map<Dependency, ArtifactVersions> lookupDependenciesUpdates( Set dependencies,
                                                                        boolean usePluginRepositories )
                                                                            throws ArtifactMetadataRetrievalException,
                                                                            InvalidVersionSpecificationException
    {
        // Create the request for details collection for parallel lookup...
        final List<Callable<DependencyArtifactVersions>> requestsForDetails =
            new ArrayList<Callable<DependencyArtifactVersions>>( dependencies.size() );
        for ( final Object dependency1 : dependencies )
        {
            final Dependency dependency = (Dependency) dependency1;
            requestsForDetails.add( new DependencyLookup( dependency, usePluginRepositories ) );
        }

        final Map<Dependency, ArtifactVersions> dependencyUpdates =
            new TreeMap<Dependency, ArtifactVersions>( new DependencyComparator() );

        // Lookup details in parallel...
        final ExecutorService executor = Executors.newFixedThreadPool( LOOKUP_PARALLEL_THREADS );
        try
        {
            final List<Future<DependencyArtifactVersions>> responseForDetails =
                executor.invokeAll( requestsForDetails );

            // Construct the final results...
            for ( final Future<DependencyArtifactVersions> details : responseForDetails )
            {
                final DependencyArtifactVersions dav = details.get();
                dependencyUpdates.put( dav.getDependency(), dav.getArtifactVersions() );
            }
        }
        catch ( final ExecutionException ee )
        {
            throw new ArtifactMetadataRetrievalException( "Unable to acquire metadata for dependencies " + dependencies
                + ": " + ee.getMessage(), ee );
        }
        catch ( final InterruptedException ie )
        {
            throw new ArtifactMetadataRetrievalException( "Unable to acquire metadata for dependencies " + dependencies
                + ": " + ie.getMessage(), ie );
        }
        finally
        {
            executor.shutdownNow();
        }
        return dependencyUpdates;
    }

    /**
     * {@inheritDoc}
     */
    public ArtifactVersions lookupDependencyUpdates( Dependency dependency, boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        getLog().debug( "Checking "
            + ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() )
            + " for updates newer than " + dependency.getVersion() );
        VersionRange versionRange = VersionRange.createFromVersionSpec( dependency.getVersion() );

        return lookupArtifactVersions( createDependencyArtifact( dependency.getGroupId(), dependency.getArtifactId(),
                                                                 versionRange, dependency.getType(),
                                                                 dependency.getClassifier(), dependency.getScope() ),
                                       usePluginRepositories );
    }

    /**
     * {@inheritDoc}
     */
    public Map<Plugin, PluginUpdatesDetails> lookupPluginsUpdates( Set<Plugin> plugins, Boolean allowSnapshots )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        // Create the request for details collection for parallel lookup...
        final List<Callable<PluginPluginUpdatesDetails>> requestsForDetails =
            new ArrayList<Callable<PluginPluginUpdatesDetails>>( plugins.size() );
        for ( final Plugin plugin : plugins )
        {
            requestsForDetails.add( new PluginLookup( plugin, allowSnapshots ) );
        }

        final Map<Plugin, PluginUpdatesDetails> pluginUpdates =
            new TreeMap<Plugin, PluginUpdatesDetails>( new PluginComparator() );

        // Lookup details in parallel...
        final ExecutorService executor = Executors.newFixedThreadPool( LOOKUP_PARALLEL_THREADS );
        try
        {
            final List<Future<PluginPluginUpdatesDetails>> responseForDetails =
                executor.invokeAll( requestsForDetails );

            // Construct the final results...
            for ( final Future<PluginPluginUpdatesDetails> details : responseForDetails )
            {
                final PluginPluginUpdatesDetails pud = details.get();
                pluginUpdates.put( pud.getPlugin(), pud.getPluginUpdatesDetails() );
            }
        }
        catch ( final ExecutionException ee )
        {
            throw new ArtifactMetadataRetrievalException( "Unable to acquire metadata for plugins " + plugins + ": "
                + ee.getMessage(), ee );
        }
        catch ( final InterruptedException ie )
        {
            throw new ArtifactMetadataRetrievalException( "Unable to acquire metadata for plugins " + plugins + ": "
                + ie.getMessage(), ie );
        }
        finally
        {
            executor.shutdownNow();
        }
        return pluginUpdates;
    }

    /**
     * {@inheritDoc}
     */
    public PluginUpdatesDetails lookupPluginUpdates( Plugin plugin, Boolean allowSnapshots )
        throws ArtifactMetadataRetrievalException, InvalidVersionSpecificationException
    {
        String version = plugin.getVersion();
        version = version == null ? "LATEST" : version;
        getLog().debug( "Checking " + ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() )
            + " for updates newer than " + version );

        VersionRange versionRange = VersionRange.createFromVersion( version );

        final boolean includeSnapshots = Boolean.TRUE.equals( allowSnapshots );

        final ArtifactVersions pluginArtifactVersions =
            lookupArtifactVersions( createPluginArtifact( plugin.getGroupId(), plugin.getArtifactId(), versionRange ),
                                    true );

        Set<Dependency> pluginDependencies = new TreeSet<Dependency>( new DependencyComparator() );
        if ( plugin.getDependencies() != null )
        {
            pluginDependencies.addAll( plugin.getDependencies() );
        }
        Map<Dependency, ArtifactVersions> pluginDependencyDetails =
            lookupDependenciesUpdates( pluginDependencies, false );

        return new PluginUpdatesDetails( pluginArtifactVersions, pluginDependencyDetails, includeSnapshots );
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
    public Map<Property, PropertyVersions> getVersionPropertiesMap( MavenProject project,
                                                                    Property[] propertyDefinitions,
                                                                    String includeProperties, String excludeProperties,
                                                                    boolean autoLinkItems )
                                                                        throws MojoExecutionException
    {
        Map<String, Property> properties = new HashMap<String, Property>();
        if ( propertyDefinitions != null )
        {
            for ( Property propertyDefinition : propertyDefinitions )
            {
                properties.put( propertyDefinition.getName(), propertyDefinition );
            }
        }
        Map<String, PropertyVersionsBuilder> builders = new HashMap<String, PropertyVersionsBuilder>();
        if ( autoLinkItems )
        {
            final PropertyVersionsBuilder[] propertyVersionsBuilders;
            try
            {
                propertyVersionsBuilders = PomHelper.getPropertyVersionsBuilders( this, project );
            }
            catch ( ExpressionEvaluationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
            catch ( IOException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }

            for ( PropertyVersionsBuilder propertyVersionsBuilder : propertyVersionsBuilders )
            {
                final String name = propertyVersionsBuilder.getName();
                builders.put( name, propertyVersionsBuilder );
                if ( !properties.containsKey( name ) )
                {
                    final Property value = new Property( name );
                    getLog().debug( "Property ${" + name + "}: Adding inferred version range of "
                        + propertyVersionsBuilder.getVersionRange() );
                    value.setVersion( propertyVersionsBuilder.getVersionRange() );
                    properties.put( name, value );
                }
            }
        }
        getLog().debug( "Searching for properties associated with builders" );
        Iterator<Property> i = properties.values().iterator();
        while ( i.hasNext() )
        {
            Property property = i.next();
            if ( includeProperties != null && !includeProperties.contains( property.getName() ) )
            {
                getLog().debug( "Skipping property ${" + property.getName() + "}" );
                i.remove();
            }
            else if ( excludeProperties != null && excludeProperties.contains( property.getName() ) )
            {
                getLog().debug( "Ignoring property ${" + property.getName() + "}" );
                i.remove();
            }
        }
        i = properties.values().iterator();
        Map<Property, PropertyVersions> propertyVersions =
            new LinkedHashMap<Property, PropertyVersions>( properties.size() );
        while ( i.hasNext() )
        {
            Property property = i.next();
            getLog().debug( "Property ${" + property.getName() + "}" );
            PropertyVersionsBuilder builder = builders.get( property.getName() );
            if ( builder == null || !builder.isAssociated() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Looks like this property is not "
                    + "associated with any dependency..." );
                builder = new PropertyVersionsBuilder( null, property.getName(), this );
            }
            if ( !property.isAutoLinkDependencies() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Removing any autoLinkDependencies" );
                builder.clearAssociations();
            }
            Dependency[] dependencies = property.getDependencies();
            if ( dependencies != null )
            {
                for ( Dependency dependency : dependencies )
                {
                    try
                    {
                        getLog().debug( "Property ${" + property.getName() + "}: Adding association to " + dependency );
                        builder.addAssociation( this.createDependencyArtifact( dependency ), false );
                    }
                    catch ( InvalidVersionSpecificationException e )
                    {
                        throw new MojoExecutionException( e.getMessage(), e );
                    }
                }
            }
            try
            {
                final PropertyVersions versions = builder.newPropertyVersions();
                if ( property.isAutoLinkDependencies() && StringUtils.isEmpty( property.getVersion() )
                    && !StringUtils.isEmpty( builder.getVersionRange() ) )
                {
                    getLog().debug( "Property ${" + property.getName() + "}: Adding inferred version range of "
                        + builder.getVersionRange() );
                    property.setVersion( builder.getVersionRange() );
                }
                versions.setCurrentVersion( project.getProperties().getProperty( property.getName() ) );
                propertyVersions.put( property, versions );
            }
            catch ( ArtifactMetadataRetrievalException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
        }
        return propertyVersions;
    }

    // This is a data container to hold the result of a Dependency lookup to its ArtifactVersions.
    private static class DependencyArtifactVersions
    {
        private final Dependency dependency;

        private final ArtifactVersions artifactVersions;

        public DependencyArtifactVersions( final Dependency dependency, final ArtifactVersions artifactVersions )
        {
            this.dependency = dependency;
            this.artifactVersions = artifactVersions;
        }

        public Dependency getDependency()
        {
            return dependency;
        }

        public ArtifactVersions getArtifactVersions()
        {
            return artifactVersions;
        }
    }

    // This is a data container to hold the result of a Dependency lookup to its ArtifactVersions.
    private static class PluginPluginUpdatesDetails
    {
        private final Plugin plugin;

        private final PluginUpdatesDetails pluginUpdatesDetails;

        public PluginPluginUpdatesDetails( final Plugin plugin, final PluginUpdatesDetails pluginUpdatesDetails )
        {
            this.plugin = plugin;
            this.pluginUpdatesDetails = pluginUpdatesDetails;
        }

        public Plugin getPlugin()
        {
            return plugin;
        }

        public PluginUpdatesDetails getPluginUpdatesDetails()
        {
            return pluginUpdatesDetails;
        }
    }

    // This Callable wraps lookupDependencyUpdates so that it can be run in parallel.
    private class DependencyLookup
        implements Callable<DependencyArtifactVersions>
    {
        private final Dependency dependency;

        private final boolean usePluginRepositories;

        public DependencyLookup( final Dependency dependency, final boolean usePluginRepositories )
        {
            this.dependency = dependency;
            this.usePluginRepositories = usePluginRepositories;
        }

        public DependencyArtifactVersions call()
            throws Exception
        {
            return new DependencyArtifactVersions( dependency,
                                                   lookupDependencyUpdates( dependency, usePluginRepositories ) );
        }
    }

    // This Callable wraps lookupPluginUpdates so that it can be run in parallel.
    private class PluginLookup
        implements Callable<PluginPluginUpdatesDetails>
    {
        private final Plugin plugin;

        private final boolean allowSnapshots;

        public PluginLookup( final Plugin plugin, final Boolean allowSnapshots )
        {
            this.plugin = plugin;
            this.allowSnapshots = allowSnapshots;
        }

        public PluginPluginUpdatesDetails call()
            throws Exception
        {
            return new PluginPluginUpdatesDetails( plugin, lookupPluginUpdates( plugin, allowSnapshots ) );
        }
    }

}
