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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.util.StringUtils;

import static java.util.Arrays.asList;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.artifact.Artifact.SCOPE_IMPORT;
import static org.apache.maven.artifact.Artifact.SCOPE_PROVIDED;
import static org.apache.maven.artifact.Artifact.SCOPE_RUNTIME;
import static org.apache.maven.artifact.Artifact.SCOPE_SYSTEM;
import static org.apache.maven.artifact.Artifact.SCOPE_TEST;

/**
 * Displays all dependencies that have newer versions available.
 * It will also display dependencies which are used by a plugin or
 * defined in the plugin within a pluginManagement.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo( name = "display-dependency-updates", requiresProject = true, requiresDirectInvocation = false )
public class DisplayDependencyUpdatesMojo
    extends AbstractVersionsDisplayMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 72;

    /**
     * Find any whitespace in a string.
     */
    private static final Pattern ALL_WHITESPACE = Pattern.compile("\\s+");
    /**
     * Keyword for scopes inclusion: include all scopes
     */
    private static final String SCOPES_KEYWORD_ALL = "all";
    /**
     * Keyword for dependencyManagement scopes inclusion: include dependency with null scope, i.e.: no scope given in dependencyManagement section.
     */
    private static final String SCOPES_KEYWORD_NULL = "null";

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.2
     */
    @Parameter( property = "processDependencyManagement", defaultValue = "true" )
    private boolean processDependencyManagement;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.2
     */
    @Parameter( property = "processDependencies", defaultValue = "true" )
    private boolean processDependencies;

    /**
     * Whether to process the dependencies sections of plugins.
     *
     * @since 2.5
     */
    @Parameter( property = "processPluginDependencies", defaultValue = "true" )
    private boolean processPluginDependencies;

    /**
     * Whether to process the dependencies sections of plugins which are defined in pluginManagement.
     *
     * @since 2.5
     */
    @Parameter( property = "processPluginDependenciesInPluginManagement", defaultValue = "true" )
    private boolean processPluginDependenciesInPluginManagement;

    /**
     * Whether to allow the major version number to be changed.
     * You need to set {@link #allowAnyUpdates} to <code>false</code> to
     * get this configuration gets control.
     * @since 2.5
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    private boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     * You need to set {@link #allowMajorUpdates} to <code>false</code> to
     * get this configuration gets control.
     *
     * @since 2.5
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    private boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     * You need to set {@link #allowMinorUpdates} to <code>false</code> to
     * get this configuration gets control.
     *
     * @since 2.5
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    private boolean allowIncrementalUpdates;

    /**
     * Whether to allow any version change to be allowed. This keeps
     * compatibility with previous versions of the plugin.
     * If you set this to false you can control changes in version
     * number by {@link #allowMajorUpdates}, {@link #allowMinorUpdates} or
     * {@link #allowIncrementalUpdates}.
     * @deprecated This will be removed with version 3.0.0
     * @since 2.5
     */
    @Parameter(property = "allowAnyUpdates", defaultValue = "true")
    private boolean allowAnyUpdates;

    /**
     * Whether to show additional information such as dependencies that do not need updating. Defaults to false.
     *
     * @since 2.1
     */
    @Parameter( property = "verbose", defaultValue = "false" )
    private boolean verbose;

    /**
     * comma-separates list of <a href="https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html#Dependency_Scope">maven dependency scopes</a>
     * (inbetween whitespace allowed) or just the keyword &quot;all&quot; (the default).
     * Only dependencies with the resolved scope (i.e.: after aplying dependencyManagement) will get checked.
     * For some use-cases it might be usefull to exclude some, most notably: system and provided.
     * The resulting dependencyScopes param would in this case be: "compile, runtime, test, import"
     *
     * @since 2.8
     */
    @Parameter( property = "dependencyScopes", defaultValue = "all" )
    private String dependencyScopes;

    /**
     * comma-separates list of <a href="https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html#Dependency_Scope">maven dependency scopes</a>
     * and the additional keyword &quot;null&quot; (inbetween whitespace allowed) or just the keyword &quot;all&quot; (the default).
     * <p>
     * Only dependencyManagement-dependencies in this scope will get checked.
     * </p>
     * <p>
     * <strong>WARNING: </strong>dependencies in the dependencyManagement section will have a null-scope if not explicitly set!
     * </p>
     *
     * @since 2.8
     */
    @Parameter( property = "dependencyManagementScopes", defaultValue = "all" )
    private String dependencyManagementScopes;

    // --------------------- GETTER / SETTER METHODS ---------------------

    private static Set<Dependency> extractPluginDependenciesFromPluginsInPluginManagement( Build build )
    {
        Set<Dependency> result = new TreeSet<>( new DependencyComparator() );
        if ( build.getPluginManagement() != null )
        {
            for ( Plugin plugin : build.getPluginManagement().getPlugins() )
            {
                if ( plugin.getDependencies() != null && !plugin.getDependencies().isEmpty() )
                {
                    for ( Dependency pluginDependency : plugin.getDependencies() )
                    {
                        result.add( pluginDependency );
                    }
                }
            }
        }
        return result;
    }

    private static Set<Dependency> extractDependenciesFromPlugins( List<Plugin> plugins )
    {
        Set<Dependency> result = new TreeSet<>( new DependencyComparator() );
        for ( Plugin plugin : plugins )
        {
            if ( plugin.getDependencies() != null && !plugin.getDependencies().isEmpty() )
            {
                for ( Dependency pluginDependency : plugin.getDependencies() )
                {
                    result.add( pluginDependency );
                }
            }
        }
        return result;
    }

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param dependencies The set of dependencies.
     * @param dependencyManagement The set of dependencies from the dependency management section.
     * @return A new set of dependencies which are from the set of dependencies but not from the set of dependency
     *         management dependencies.
     * @since 1.0-beta-1
     */
    private static Set<Dependency> removeDependencyManagment( Set<Dependency> dependencies, Set<Dependency> dependencyManagement )
    {
        Set<Dependency> result = new TreeSet<>( new DependencyComparator() );
        for ( Iterator<Dependency> i = dependencies.iterator(); i.hasNext(); )
        {
            Dependency c = i.next();
            boolean matched = false;
            Iterator<Dependency> j = dependencyManagement.iterator();
            while ( !matched && j.hasNext() )
            {
                Dependency t =j.next();
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() )
                    && StringUtils.equals( t.getArtifactId(), c.getArtifactId() )
                    && ( t.getScope() == null || StringUtils.equals( t.getScope(), c.getScope() ) )
                    && ( t.getClassifier() == null || StringUtils.equals( t.getClassifier(), c.getClassifier() ) )
                    && ( c.getVersion() == null || t.getVersion() == null
                        || StringUtils.equals( t.getVersion(), c.getVersion() ) ) )
                {
                    matched = true;
                    break;
                }
            }
            if ( !matched )
            {
                result.add( c );
            }
        }
        return result;
    }

    public boolean isProcessingDependencyManagement()
    {
        return processDependencyManagement;
    }

    public boolean isProcessingDependencies()
    {
        return processDependencies;
    }

    public boolean isProcessingPluginDependencies()
    {
        return processPluginDependencies;
    }

    public boolean isProcessPluginDependenciesInDependencyManagement()
    {
        return processPluginDependenciesInPluginManagement;
    }

    public boolean isVerbose()
    {
        return verbose;
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * parses a comma-separated string of scopes with optional whitespace inbetween.
     * Also cares for our internal scope keywords: {@link #SCOPES_KEYWORD_ALL} and {@link #SCOPES_KEYWORD_NULL}.
     */
    private static Set<String> parseScopesString( String scopesString ) {
        Set<String> result = new HashSet<>();

        String normalizedScopesString = ( scopesString == null ? SCOPES_KEYWORD_ALL : scopesString )
                .trim()
                .toLowerCase( Locale.US) ;

        if ( SCOPES_KEYWORD_ALL.equals(scopesString) ) {
            result.addAll(asList(SCOPE_COMPILE,
                                 SCOPE_PROVIDED,
                                 SCOPE_RUNTIME,
                                 SCOPE_TEST,
                                 SCOPE_SYSTEM,
                                 SCOPE_IMPORT,
                                 SCOPES_KEYWORD_NULL));
        } else {
            String[] scopes = ALL_WHITESPACE
                    .matcher( normalizedScopesString ).replaceAll( "" )
                    .split(",");

            result.addAll( asList( scopes ) );
        }

        return result;
    }

    private Set<Dependency> removeUnwantedScopes( Set<Dependency> dependencies, String scopesString ) {
        TreeSet<Dependency> filtered = new TreeSet<>( new DependencyComparator() );

        Set<String> scopes = parseScopesString( scopesString );
        getLog().debug( "Scopes: " + scopes );

        for ( Dependency dependency : dependencies ) {
            boolean inScope = isInScope(scopes, dependency);
            getLog().debug( "Dependency:" + dependency + " with scope: " + dependency.getScope() + ", include: " + inScope );
            if ( inScope ) {
                filtered.add( dependency );
            }
        }

        return filtered;
    }

    /**
     * treats dependencies with null scope as if having a scope of &quot;null&qout; (see {@link #SCOPES_KEYWORD_NULL}.
     */
    private static boolean isInScope( Set<String> scopes, Dependency dependency ) {
        return scopes.contains( String.valueOf( dependency.getScope() ) );
    }

    /**
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException when things go wrong in a very bad way
     * @see AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        logInit();

        Set<Dependency> dependencyManagement = new TreeSet<>( new DependencyComparator() );
        if ( getProject().getDependencyManagement() != null )
        {

            List<Dependency> dependenciesFromPom = getProject().getDependencyManagement().getDependencies();
            for ( Dependency dependency : dependenciesFromPom )
            {
                getLog().debug( "dependency from pom: " + dependency.getGroupId() + ":" + dependency.getArtifactId()
                    + ":" + dependency.getVersion() + ":" + dependency.getScope() );
                if ( dependency.getVersion() == null )
                {
                    // get parent and get the information from there.
                    if ( getProject().hasParent() )
                    {
                        getLog().debug( "Reading parent dependencyManagement information" );
                        if ( getProject().getParent().getDependencyManagement() != null )
                        {
                            List<Dependency> parentDeps =
                                getProject().getParent().getDependencyManagement().getDependencies();
                            for ( Dependency parentDep : parentDeps )
                            {
                                // only groupId && artifactId needed cause version is null
                                if ( dependency.getGroupId().equals( parentDep.getGroupId() )
                                    && dependency.getArtifactId().equals( parentDep.getArtifactId() )
                                    && dependency.getType().equals( parentDep.getType() ) )
                                {
                                    dependencyManagement.add( parentDep );
                                }
                            }
                        }
                    }
                    else
                    {
                        String message = "We can't get the version for the dependency " + dependency.getGroupId() + ":"
                            + dependency.getArtifactId() + " cause there does not exist a parent.";
                        getLog().error( message );
                        // Throw error cause we will not able to get a version for a dependency.
                        throw new MojoExecutionException( message );
                    }
                }
                else
                {
                    dependencyManagement.add( dependency );
                }
            }
        }

        Set<Dependency> dependencies = new TreeSet<>( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );

        if ( isProcessingDependencyManagement() )
        {
            dependencies = removeDependencyManagment( dependencies, dependencyManagement );
        }

		Set<Dependency> pluginDependencies = new TreeSet<>( new DependencyComparator() );

        if ( isProcessingPluginDependencies() )
        {
            pluginDependencies = extractDependenciesFromPlugins( getProject().getBuildPlugins() );
        }

        Set<Dependency> pluginDependenciesInPluginManagement = new TreeSet<>( new DependencyComparator() );
        if ( isProcessPluginDependenciesInDependencyManagement() )
        {
            pluginDependenciesInPluginManagement =
                extractPluginDependenciesFromPluginsInPluginManagement( getProject().getBuild() );
        }

        try
        {
            if ( isProcessingDependencyManagement() )
            {
                dependencyManagement = removeUnwantedScopes( dependencyManagement, dependencyManagementScopes );
                logUpdates( getHelper().lookupDependenciesUpdates( dependencyManagement, false ),
                            "Dependency Management" );
            }
            if ( isProcessingDependencies() )
            {
                dependencies = removeUnwantedScopes( dependencies, dependencyScopes );
                logUpdates( getHelper().lookupDependenciesUpdates( dependencies, false ), "Dependencies" );
            }
            if ( isProcessPluginDependenciesInDependencyManagement() )
            {
                logUpdates( getHelper().lookupDependenciesUpdates( pluginDependenciesInPluginManagement, false ),
                            "pluginManagement of plugins" );
            }
            if ( isProcessingPluginDependencies() )
            {
                logUpdates( getHelper().lookupDependenciesUpdates( pluginDependencies, false ), "Plugin Dependencies" );
            }
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

	private UpdateScope calculateUpdateScope()
    {
        UpdateScope result = UpdateScope.ANY;
        if ( !allowAnyUpdates )
        {
            if ( allowMajorUpdates )
            {
                result = UpdateScope.MAJOR;
            }
            else
            {
                if ( allowMinorUpdates )
                {
                    result = UpdateScope.MINOR;
                }
                else
                {
                    if ( allowIncrementalUpdates )
                    {
                        result = UpdateScope.INCREMENTAL;
                    }
                }
            }
        }
        return result;
    }

    private void logUpdates( Map<Dependency, ArtifactVersions> updates, String section )
    {
        List<String> withUpdates = new ArrayList<>();
        List<String> usingCurrent = new ArrayList<>();
        Iterator i = updates.values().iterator();
        while ( i.hasNext() )
        {
            ArtifactVersions versions = (ArtifactVersions) i.next();
            String left = "  " + ArtifactUtils.versionlessKey( versions.getArtifact() ) + " ";
            final String current;
            ArtifactVersion latest;
            if ( versions.isCurrentVersionDefined() )
            {
                current = versions.getCurrentVersion().toString();
                latest = versions.getNewestUpdate( calculateUpdateScope(), allowSnapshots );
            }
            else
            {
                ArtifactVersion newestVersion =
                    versions.getNewestVersion( versions.getArtifact().getVersionRange(), allowSnapshots );
                current = versions.getArtifact().getVersionRange().toString();
                latest = newestVersion == null ? null
                                : versions.getNewestUpdate( newestVersion, calculateUpdateScope(), allowSnapshots );
                if ( latest != null
                    && ArtifactVersions.isVersionInRange( latest, versions.getArtifact().getVersionRange() ) )
                {
                    latest = null;
                }
            }
            String right = " " + ( latest == null ? current : current + " -> " + latest.toString() );
            List<String> t = latest == null ? usingCurrent : withUpdates;
            if ( right.length() + left.length() + 3 > INFO_PAD_SIZE )
            {
                t.add( left + "..." );
                t.add( StringUtils.leftPad( right, INFO_PAD_SIZE ) );

            }
            else
            {
                t.add( StringUtils.rightPad( left, INFO_PAD_SIZE - right.length(), "." ) + right );
            }
        }

        if ( isVerbose() )
        {
            if ( usingCurrent.isEmpty() )
            {
                if ( !withUpdates.isEmpty() )
                {
                    logLine( false, "No dependencies in " + section + " are using the newest version." );
                    logLine( false, "" );
                }
            }
            else
            {
                logLine( false, "The following dependencies in " + section + " are using the newest version:" );
                i = usingCurrent.iterator();
                while ( i.hasNext() )
                {
                    logLine( false, (String) i.next() );
                }
                logLine( false, "" );
            }
        }


        if ( withUpdates.isEmpty() )
        {
            if ( !usingCurrent.isEmpty() )
            {
                logLine( false, "No dependencies in " + section + " have newer versions." );
                logLine( false, "" );
            }
        }
        else
        {
            logLine( false, "The following dependencies in " + section + " have newer versions:" );
            i = withUpdates.iterator();
            while ( i.hasNext() )
            {
                logLine( false, (String) i.next() );
            }
            logLine( false, "" );
        }
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException when things go wrong in a very bad way
     * @throws XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }

}
