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

import javax.inject.Inject;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;

/**
 * Abstract base class for the Display___ mojos.
 *
 * @author Stephen Connolly
 */
public abstract class AbstractVersionsDisplayMojo
    extends AbstractVersionsUpdaterMojo
{
    static final String NL = System.getProperty( "line.separator" );

    private static final int DEFAULT_OUTPUT_LINE_WIDTH = 80;

    /**
     * If specified then the display output will be sent to the specified file.
     *
     * @since 2.2
     */
    @Parameter( property = "versions.outputFile" )
    protected File outputFile;

    /**
     * Controls whether the display output is logged to the console.
     *
     * @since 2.2
     */
    @Parameter( property = "versions.logOutput", defaultValue = "true" )
    protected boolean logOutput;

    /**
     * The character encoding to use when writing to {@link #outputFile}.
     *
     * @since 2.2
     */
    @Parameter( property = "outputEncoding", defaultValue = "${project.reporting.outputEncoding}" )
    protected String outputEncoding;

    /**
     * Line width which should be used to format the padding of the version info list output.
     *
     * @since 2.10.0
     */
    @Parameter( property = "versions.outputLineWidth",
                defaultValue = AbstractVersionsDisplayMojo.DEFAULT_OUTPUT_LINE_WIDTH + "" )
    protected int outputLineWidth;

    private boolean outputFileError = false;

    @Inject
    protected AbstractVersionsDisplayMojo( RepositorySystem repositorySystem,
                                           MavenProjectBuilder projectBuilder,
                                           ArtifactMetadataSource artifactMetadataSource,
                                           WagonManager wagonManager,
                                           ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    protected void logInit()
    {
        if ( outputFile != null && !outputFileError )
        {
            if ( outputFile.isFile() )
            {
                final String key = AbstractVersionsDisplayMojo.class.getName() + ".outputFile";
                String outputFileName;
                try
                {
                    outputFileName = outputFile.getCanonicalPath();
                }
                catch ( IOException e )
                {
                    outputFileName = outputFile.getAbsolutePath();
                }
                Set<String> files = (Set<String>) getPluginContext().get( key );
                if ( files == null )
                {
                    files = new LinkedHashSet<>();
                }
                else
                {
                    files = new LinkedHashSet<>( files );
                }
                if ( !files.contains( outputFileName ) )
                {
                    if ( !outputFile.delete() )
                    {
                        getLog().error( "Cannot delete " + outputFile + " will append instead" );
                    }
                }
                files.add( outputFileName );
                getPluginContext().put( key, files );
            }
            else
            {
                if ( outputFile.exists() )
                {
                    getLog().error( "Cannot send output to " + outputFile + " as it exists but is not a file" );
                    outputFileError = true;
                }
                else if ( !outputFile.getParentFile().isDirectory() )
                {
                    if ( !outputFile.getParentFile().mkdirs() )
                    {
                        outputFileError = true;
                    }
                }
            }
            if ( !outputFileError && StringUtils.isBlank( outputEncoding ) )
            {
                outputEncoding = System.getProperty( "file.encoding" );
                getLog().warn( "File encoding has not been set, using platform encoding " + outputEncoding
                                   + ", i.e. build is platform dependent!" );
            }
        }
    }

    protected void logLine( boolean error, String line )
    {
        if ( logOutput )
        {
            if ( error )
            {
                getLog().error( line );
            }
            else
            {
                getLog().info( line );
            }
        }
        if ( outputFile != null && !outputFileError )
        {
            try
            {
                Files.write( outputFile.toPath(),
                             ( error ? "> " + line + NL : line + NL ).getBytes( outputEncoding ),
                             StandardOpenOption.APPEND, StandardOpenOption.CREATE );
            }
            catch ( IOException e )
            {
                getLog().error( "Cannot send output to " + outputFile, e );
                outputFileError = true;
            }
        }
    }

    /**
     * @return Offset of the configured output line width compared to the default with of 80.
     */
    protected int getOutputLineWidthOffset()
    {
        return this.outputLineWidth - DEFAULT_OUTPUT_LINE_WIDTH;
    }

}
