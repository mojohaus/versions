package org.codehaus.mojo.versions.utils;

import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.logging.Logger;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.path.PathTranslator;
import org.apache.maven.project.MavenProject;

import java.util.Properties;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 17-Mar-2009 08:51:42
 */
public class VersionsExpressionEvaluator
    extends PluginParameterExpressionEvaluator
    implements ExpressionEvaluator
{
    public VersionsExpressionEvaluator( MavenSession mavenSession, PathTranslator pathTranslator,
                                        MavenProject mavenProject )
    {
        super( mavenSession, new MojoExecution( new MojoDescriptor() ), pathTranslator, null, mavenProject,
               mavenSession.getExecutionProperties() );
    }
}
