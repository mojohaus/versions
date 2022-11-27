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

import java.io.File;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.plexus.i18n.I18N;

/**
 * Base class for all versions reports.
 *
 * @param <T> modelled report object
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public abstract class AbstractVersionsReport<T>
    extends AbstractMavenReport
{
    /**
     * Internationalization component.
     *
     * @since 1.0-alpha-3
     */
    protected I18N i18n;

    protected RepositorySystem repositorySystem;

    /**
     * Skip entire check.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "versions.skip" )
    private boolean skip;

    /**
     * The (injected) {@link org.eclipse.aether.RepositorySystem aetherRepositorySystem} instance.
     */
    protected org.eclipse.aether.RepositorySystem aetherRepositorySystem;

    /**
     * settings.xml's server id for the URL. This is used when wagon needs extra authentication information.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "maven.version.rules.serverId", defaultValue = "serverId" )
    private String serverId;

    /**
     * URI of a ruleSet file containing the rules that control how to compare
     * version numbers. The URI could be either a Wagon URI or a classpath URI
     * (e.g. <code>classpath:///package/sub/package/rules.xml</code>).
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "maven.version.rules" )
    private String rulesUri;

    /**
     * The versioning rule to use when comparing versions. Valid values are <code>maven</code>, <code>numeric</code>
     * which will handle long version numbers provided all components are numeric, or <code>mercury</code> which will
     * use the mercury version number comparison rules.
     *
     * @since 1.0-alpha-1
     */
    @Parameter( property = "comparisonMethod" )
    protected String comparisonMethod;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "allowSnapshots", defaultValue = "false" )
    protected boolean allowSnapshots;

    /**
     * Our versions helper.
     */
    private VersionsHelper helper;

    /**
     * The Maven Session.
     *
     * @since 1.0-beta-1
     */
    @Parameter( defaultValue = "${session}", required = true, readonly = true )
    protected MavenSession session;

    @Parameter( defaultValue = "${mojoExecution}", required = true, readonly = true )
    private MojoExecution mojoExecution;

    /**
     * <p>Allows specifying the {@linkplain RuleSet} object describing rules
     * on artifact versions to ignore when considering updates.</p>
     *
     * @see <a href="https://www.mojohaus.org/versions-maven-plugin/version-rules.html#Using_the_ruleSet_element_in_the_POM">
     *     Using the ruleSet element in the POM</a>
     *
     * @since 2.13.0
     */
    @Parameter
    protected RuleSet ruleSet;

    /**
     * <p>Allows specifying ignored versions directly as an alternative
     * to providing the {@linkplain #ruleSet} parameter; mainly created
     * for {@code -D} property usage.</p>
     *
     * <p>
     * Example: {@code "1\.0\.1,.+-M.,.*-SNAPSHOT"}
     * </p>
     *
     * <p><em>Currently, this parameter will override the defined {@link #ruleSet}</em></p>
     * @since 2.13.0
     */
    @Parameter( property = "maven.version.ignore" )
    protected Set<String> ignoredVersions;

    /**
     * Renderer factory
     *
     * @since 2.13.0
     */
    protected ReportRendererFactory rendererFactory;

    /**
     * (injected) map of {@link Wagon} instances
     *
     * @since 2.14.0
     */
    protected Map<String, Wagon> wagonMap;

    // --------------------- GETTER / SETTER METHODS ---------------------

    protected AbstractVersionsReport( I18N i18n, RepositorySystem repositorySystem,
                                      org.eclipse.aether.RepositorySystem aetherRepositorySystem,
                                      Map<String, Wagon> wagonMap,
                                      ReportRendererFactory rendererFactory )
    {
        this.i18n = i18n;
        this.repositorySystem = repositorySystem;
        this.aetherRepositorySystem = aetherRepositorySystem;
        this.wagonMap = wagonMap;
        this.rendererFactory = rendererFactory;
    }

    public VersionsHelper getHelper()
        throws MavenReportException
    {
        if ( helper == null )
        {
            try
            {
                helper = new DefaultVersionsHelper.Builder()
                        .withRepositorySystem( repositorySystem )
                        .withAetherRepositorySystem( aetherRepositorySystem )
                        .withWagonMap( wagonMap )
                        .withServerId( serverId )
                        .withRulesUri( rulesUri )
                        .withRuleSet( ruleSet )
                        .withIgnoredVersions( ignoredVersions )
                        .withLog( getLog() )
                        .withMavenSession( session )
                        .withMojoExecution( mojoExecution )
                        .build();
            }
            catch ( MojoExecutionException e )
            {
                throw new MavenReportException( e.getMessage(), e );
            }
        }
        return helper;
    }

    /**
     * {@inheritDoc}
     */
    protected void executeReport( Locale locale )
        throws MavenReportException
    {
        if ( !skip )
        {
            try
            {
                doGenerateReport( locale, getSink() );
            }
            catch ( MojoExecutionException e )
            {
                throw new MavenReportException( e.getMessage(), e );
            }
        }
    }

    /**
     * generates the report.
     *
     * @param locale the locale to generate the report for.
     * @param sink the report formatting tool.
     * @throws MavenReportException when things go wrong.
     * @throws MojoExecutionException if something goes wrong.
     */
    protected abstract void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException, MojoExecutionException;

    @Override
    protected MavenProject getProject()
    {
        return project;
    }

    @Override
    protected String getOutputDirectory()
    {
        if ( !outputDirectory.isAbsolute() )
        {
            outputDirectory = new File( project.getBasedir(), outputDirectory.getPath() );
        }

        return outputDirectory.getAbsolutePath();
    }

    @Override
    protected Renderer getSiteRenderer()
    {
        return siteRenderer;
    }

    @Override
    public String getDescription( Locale locale )
    {
        return getText( locale, "report.description" );
    }

    @Override
    public String getName( Locale locale )
    {
        return getText( locale, "report.title" );
    }

    /**
     * Gets the localized message for this report.
     *
     * @param locale the locale.
     * @param key the message key.
     * @return the message.
     */
    public String getText( Locale locale, String key )
    {
        return i18n.getString( getOutputName(), locale, key );
    }

    public Boolean getAllowSnapshots()
    {
        return this.allowSnapshots;
    }

    public String getComparisonMethod()
    {
        return comparisonMethod;
    }

    public I18N getI18n()
    {
        return i18n;
    }

}
