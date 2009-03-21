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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.utils.VersionsExpressionEvaluator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.model.Model;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.path.PathTranslator;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal test
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class TestMojo
    extends AbstractVersionsUpdaterMojo
{
    /**
     * The Maven Project.
     *
     * @parameter expression="${session}"
     * @required
     * @readonly
     * @since 1.0-alpha-1
     */
    private MavenSession session;

    /**
     * @component
     */
    private PathTranslator pathTranslator;

    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            PropertyVersions[] v = PomHelper.getPropertyVersions( getHelper(), getProject(),
                                                                  new VersionsExpressionEvaluator( session,
                                                                                                   pathTranslator,
                                                                                                   getProject() ) );
            for ( int i = 0; i < v.length; i++ )
            {
                final ArtifactVersion[] versions = v[i].getVersions( Boolean.TRUE.equals( allowSnapshots ) );
                final String currentVersion = getProject().getProperties().getProperty( v[i].getName() );

                if ( versions.length > 0 && !currentVersion.equals( versions[versions.length - 1].toString() ) )
                {
                    final String newVersion = versions[versions.length - 1].toString();
                    if ( PomHelper.setPropertyVersion( pom, v[i].getProfileId(), v[i].getName(), newVersion ) )
                    {
                        StringBuffer logMessage = new StringBuffer();
                        if ( v[i].getProfileId() != null )
                        {
                            logMessage.append( "Profile " );
                            logMessage.append( v[i].getProfileId() );
                            logMessage.append( ": " );
                        }
                        logMessage.append( "Updated property ${" );
                        logMessage.append( v[i].getName() );
                        logMessage.append( '}' );
                        logMessage.append( " from \"" );
                        logMessage.append( currentVersion );
                        logMessage.append( "\" to \"" );
                        logMessage.append( newVersion );
                        logMessage.append( '\"' );

                        getLog().info( logMessage.toString() );
                    }
                }
            }

        }
        catch ( IOException e )
        {
            getLog().info( e );
        }
        catch ( ExpressionEvaluationException e )
        {
            getLog().info( e );
        }


    }

}
