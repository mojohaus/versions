package org.codehaus.mojo.versions;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.utils.DependencyComparator;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @goal dependency-updates-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 31-Jan-2009 10:32:10
 */
public class DependencyUpdatesReport
    extends AbstractVersionsReport
{
    public boolean isExternalReport()
    {
        return false;
    }

    public boolean canGenerateReport()
    {
        return true;
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException, MojoExecutionException
    {
        Map/*<Dependency,List<ArtifactVersion>*/ dependencyUpdates = new TreeMap( new DependencyComparator() );
        Set dependencies = new TreeSet( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );
        Iterator i = dependencies.iterator();
        while ( i.hasNext() )
        {
            Dependency dependency = (Dependency) i.next();
            String groupId = dependency.getGroupId();
            String artifactId = dependency.getArtifactId();
            String version = dependency.getVersion();
            getLog().debug( "Checking " + groupId + ":" + artifactId + " for updates newer than " + version );

            VersionRange versionRange = null;
            try
            {
                versionRange = VersionRange.createFromVersionSpec( version );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MavenReportException( "Invalid version range specification: " + version, e );
            }

            Artifact artifact =
                artifactFactory.createDependencyArtifact( groupId, artifactId, versionRange, dependency.getType(),
                                                          dependency.getClassifier(), dependency.getScope() );

            ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifact, false );
            dependencyUpdates.put( dependency,
                                   Arrays.asList( artifactVersions.getNewerVersions( dependency.getVersion() ) ) );

        }
        DependencyUpdatesRenderer renderer =
            new DependencyUpdatesRenderer( sink, getI18n(), getOutputName(), locale, dependencyUpdates );
        renderer.render();
    }

    public String getOutputName()
    {
        return "dependency-updates-report";
    }

}