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
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.FileUtils;

/**
 * Abstract base class for the Display___ mojos.
 *
 * @author Stephen Connolly
 */
public abstract class AbstractVersionsDisplayMojo
    extends AbstractVersionsUpdaterMojo
{
    /**
     * If specified then the display output will be sent to the specified file.
     *
     * @since 2.2
     */
    @Parameter( property = "versions.outputFile" )
    private File outputFile;

    /**
     * Controls whether the display output is logged to the console.
     *
     * @since 2.2
     */
    @Parameter( property = "versions.logOutput", defaultValue = "true" )
    private boolean logOutput;

    /**
     * The character encoding to use when writing to {@link #outputFile}.
     *
     * @since 2.2
     */
    @Parameter( property = "outputEncoding", defaultValue = "${project.reporting.outputEncoding}" )
    private String outputEncoding;

    private boolean outputFileError = false;

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
                FileUtils.fileAppend( outputFile.getAbsolutePath(), outputEncoding,
                                      error ? "> " + line + System.getProperty( "line.separator" )
                                                      : line + System.getProperty( "line.separator" ) );
            }
            catch ( IOException e )
            {
                getLog().error( "Cannot send output to " + outputFile, e );
                outputFileError = true;
            }
        }
    }

}
