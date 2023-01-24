package org.codehaus.mojo.versions.utils;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.apache.maven.model.Extension;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.model.io.xpp3.CoreExtensionsXpp3Reader;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

/**
 * Utilities for reading and handling core extensions.
 *
 * @author Andrzej Jarmoniuk
 * @since 2.15.0
 */
public final class CoreExtensionUtils {
    /**
     * Reads the core extensions (not build extensions) configured for the given project
     * from the {@code ${project}/.mvn/extensions.xml} file.
     *
     * @param project {@link MavenProject} instance
     * @return stream of core extensions defined in the {@code ${project}/.mvn/extensions.xml} file
     * @throws IOException thrown if a file I/O operation fails
     * @throws XmlPullParserException thrown if the file cannot be parsed
     * @since 2.15.0
     */
    public static Stream<Extension> getCoreExtensions(MavenProject project) throws IOException, XmlPullParserException {
        Path extensionsFile = project.getBasedir().toPath().resolve(".mvn/extensions.xml");
        if (!Files.isRegularFile(extensionsFile)) {
            return Stream.empty();
        }

        try (Reader reader = new BufferedReader(new InputStreamReader(Files.newInputStream(extensionsFile)))) {
            return new CoreExtensionsXpp3Reader()
                    .read(reader).getExtensions().stream().map(ex -> ExtensionBuilder.newBuilder()
                            .withGroupId(ex.getGroupId())
                            .withArtifactId(ex.getArtifactId())
                            .withVersion(ex.getVersion())
                            .build());
        }
    }
}
