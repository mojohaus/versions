package org.codehaus.mojo.versions.utils;
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

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

import static java.nio.file.FileVisitResult.CONTINUE;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.apache.commons.text.CaseUtils.toCamelCase;

/**
 * Auxiliary test utilities
 */
public class TestUtils
{
    /**
     * Creates a temporary directory with the given name
     *
     * @param name name of the directory to create
     * @return {@linkplain Path} object pointing to the directory
     * @throws IOException should the I/O operation fail
     */
    public static Path createTempDir( String name ) throws IOException
    {
        return Files.createTempDirectory( toCamelCase( name, false ) );
    }

    /**
     * Deletes the given directory together with all its contents
     *
     * @param dir directory to delete
     * @throws IOException should an I/O operation fail
     */
    public static void tearDownTempDir( Path dir ) throws IOException
    {
        if ( dir != null && Files.exists( dir ) )
        {
            Files.walkFileTree( dir, new SimpleFileVisitor<Path>()
            {
                @Override
                public FileVisitResult visitFile( Path file, BasicFileAttributes attrs ) throws IOException
                {
                    Files.delete( file );
                    return CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory( Path dir, IOException exc ) throws IOException
                {
                    Files.delete( dir );
                    return CONTINUE;
                }
            } );
        }
    }

    /**
     * Copies the {@code src} directory to {@code dst} recursively,
     * creating the missing directories if necessary
     *
     * @param src source directory path
     * @param dst destination directory path
     * @throws IOException should an I/O error occur
     */
    public static void copyDir( Path src, Path dst ) throws IOException
    {
        Files.walkFileTree( src, new SimpleFileVisitor<Path>()
        {
            @Override
            public FileVisitResult preVisitDirectory( Path dir, BasicFileAttributes attrs )
                    throws IOException
            {
                Files.createDirectories( dst.resolve( src.relativize( dir ) ) );
                return CONTINUE;
            }

            @Override
            public FileVisitResult visitFile( Path file, BasicFileAttributes attrs )
                    throws IOException
            {
                Files.copy( file, dst.resolve( src.relativize( file ) ), REPLACE_EXISTING );
                return CONTINUE;
            }
        } );
    }
}
