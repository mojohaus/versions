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

import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Attempts to resolve unlocked snapshot dependency versions to the locked timestamp versions used in the build. For
 * example, an unlocked snapshot version like "1.0-SNAPSHOT" could be resolved to "1.0-20090128.202731-1". If a
 * timestamped snapshot is not available, then the version will remained unchanged. This would be the case if the
 * dependency is only available in the local repository and not in a remote snapshot repository.
 *
 * @author Paul Gier
 * @goal unlock-snapshots
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class UnlockSnapshotsMojo
    extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a timestamped snapshot version. For example 1.0-20090128.202731-1
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "-\\d{8}.\\d{6}-\\d+" );

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {

        if ( getProject().getDependencyManagement() != null )
        {
            unlockSnapshots( pom, getProject().getDependencyManagement().getDependencies() );
        }
        unlockSnapshots( pom, getProject().getDependencies() );
    }

    private void unlockSnapshots( ModifiedPomXMLEventReader pom, List dependencies )
        throws XMLStreamException
    {
        Iterator iter = dependencies.iterator();
        while ( iter.hasNext() )
        {
            Dependency dep = (Dependency) iter.next();
            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( versionMatcher.find() && versionMatcher.end() == version.length() )
            {
                String unlockedVersion = versionMatcher.replaceFirst( "-SNAPSHOT" );
                if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), dep.getVersion(),
                                                     unlockedVersion ) )
                {
                    getLog().debug( "Version set to " + unlockedVersion + " for dependnecy: " + dep );
                }
            }
        }
    }

}
