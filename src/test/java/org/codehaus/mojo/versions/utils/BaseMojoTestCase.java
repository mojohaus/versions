package org.codehaus.mojo.versions.utils;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.codehaus.plexus.util.ReaderFactory;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

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

/**
 * <p>Extension of the {@link AbstractMojoTestCase} class, providing support
 * for loading projects specified by POM file path (instead of
 * using stubs with <i>implementation</i> hint).</p>
 *
 * <p>Example usage:</p>
 *
 * <pre>
 *     // provide the path to the POM containing the tested project
 *     SetMojo myMojo = createMojo( "set", "src/test/resources/org/codehaus/mojo/set/versionless-01/pom.xml" );
 *     assertNotNull( myMojo );
 *     myMojo.execute();
 * </pre>
 * @author Andrzej Jarmoniuk
 */
public abstract class BaseMojoTestCase extends AbstractMojoTestCase
{
    /**
     * Lookup the mojo leveraging the actual subprojects pom
     * and injects the project using the given pom file path.
     *
     * @param goal        to execute on the plugin
     * @param pomFilePath path to the pom project to inject
     * @return a Mojo instance
     * @throws Exception thrown if mojo lookup fails
     */
    @SuppressWarnings( "unchecked" )
    protected <T extends Mojo> T createMojo( String goal, String pomFilePath ) throws Exception
    {
        File pomFile = new File( pomFilePath );
        T mojo = (T) lookupMojo( goal, pomFile );
        setVariableValueToObject( mojo, "project", new TestProjectStub( pomFile ) );
        return mojo;
    }

    private static class TestProjectStub extends MavenProjectStub
    {
        private final File pomFile;

        /**
         * Default constructor
         */
        private TestProjectStub( File pomFile ) throws IOException, XmlPullParserException
        {
            this.pomFile = pomFile;
            MavenXpp3Reader pomReader = new MavenXpp3Reader();
            Model model = pomReader.read( ReaderFactory.newXmlReader( pomFile ) );
            model.setPomFile( pomFile );

            setModel( model );
            setOriginalModel( model.clone() );
            setGroupId( model.getGroupId() );
            setArtifactId( model.getArtifactId() );
            setVersion( model.getVersion() );
            setName( model.getName() );
            setUrl( model.getUrl() );
            setPackaging( model.getPackaging() );
            setFile( model.getPomFile() );

            setBuild( new Build()
            {{
                setFinalName( model.getArtifactId() );
                setDirectory( getBasedir() + "/target" );
                setSourceDirectory( getBasedir() + "/src/main/java" );
                setOutputDirectory( getBasedir() + "/target/classes" );
                setTestSourceDirectory( getBasedir() + "/src/test/java" );
                setTestOutputDirectory( getBasedir() + "/target/test-classes" );
            }} );

            setCompileSourceRoots( Collections.singletonList( getBasedir() + "/src/main/java" ) );
            setTestCompileSourceRoots( Collections.singletonList( getBasedir() + "/src/test/java" ) );
        }

        @Override
        public File getBasedir()
        {
            return pomFile.getParentFile();
        }

        @Override
        public Properties getProperties()
        {
            return getModel().getProperties();
        }

        @Override
        public List<Dependency> getDependencies()
        {
            return getModel().getDependencies();
        }

        @Override
        public DependencyManagement getDependencyManagement()
        {
            return getModel().getDependencyManagement();
        }
    }
}
