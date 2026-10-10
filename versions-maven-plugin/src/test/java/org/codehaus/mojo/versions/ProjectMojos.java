package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.nio.file.Path;

import org.apache.maven.plugin.Mojo;

/**
 * Looks up mojos from code, for tests that pick the POM at run time or test a reactor. Unlike {@code @InjectMojo}
 * in a {@link ProjectMojoTest}, the POM is not copied: a goal that rewrites it changes the given file. Take it as a
 * parameter of a test or {@code @BeforeEach} method.
 */
interface ProjectMojos {

    /**
     * The mojo of {@code goal}, configured from {@code pom} (a POM file, or a directory with a {@code pom.xml}) and
     * working on the project built from it.
     */
    <T extends Mojo> T lookup(String goal, Path pom) throws Exception;

    /**
     * The mojo of {@code goal} in a reactor of the {@code modules} of {@code baseDir}, running in the first module:
     * the session's projects are the modules, and all its projects are the project in {@code baseDir}, if there is
     * one, followed by the modules.
     */
    <T extends Mojo> T lookupInReactor(String goal, Path baseDir, String... modules) throws Exception;
}
