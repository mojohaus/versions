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

import javax.xml.stream.XMLStreamException;
import javax.xml.transform.TransformerException;

import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.model.Build;
import org.apache.maven.model.Extension;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.model.io.stax.CoreExtensionsStaxReader;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;

import static java.util.Optional.ofNullable;

/**
 * Utilities for reading and handling extensions.
 */
public final class ExtensionUtils {

    /**
     * A private constructor for utils class.
     */
    ExtensionUtils() {}

    /**
     * Reads the core extensions configured for the given project
     * from the {@code ${project}/.mvn/extensions.xml} file.
     *
     * @param project {@link MavenProject} instance
     * @return stream of core extensions defined in the {@code ${project}/.mvn/extensions.xml} file
     * @throws IOException thrown if a file I/O operation fails
     * @throws XMLStreamException thrown if the file cannot be parsed
     * @since 2.15.0
     */
    public static Stream<Extension> getCoreExtensions(MavenProject project) throws IOException, XMLStreamException {
        Path extensionsFile = project.getBasedir().toPath().resolve(".mvn/extensions.xml");
        if (!Files.isRegularFile(extensionsFile)) {
            return Stream.empty();
        }

        try (Reader reader = Files.newBufferedReader(extensionsFile, StandardCharsets.UTF_8)) {
            return new CoreExtensionsStaxReader()
                    .read(reader).getExtensions().stream().map(ex -> ExtensionBuilder.newBuilder()
                            .withGroupId(ex.getGroupId())
                            .withArtifactId(ex.getArtifactId())
                            .withVersion(ex.getVersion())
                            .build());
        }
    }

    /**
     * Returns a stream of build extensions configured for the given project
     * @param project {@link MavenProject} instance
     * @param log {@link Log} instance
     * @param interpolateProperties when {@code false}, will return extensions based on raw model, otherwise will
     *                              process the interpolated model
     * @return stream of build extensions
     * @throws IOException if the model file can't be read
     * @throws XMLStreamException if the model file can't be parsed
     * @throws TransformerException if the model file can't be parsed
     */
    public static Stream<Extension> getBuildExtensions(MavenProject project, Log log, boolean interpolateProperties)
            throws XMLStreamException, IOException, TransformerException {
        if (interpolateProperties) {
            return getInterpolatedBuildExtensions(project, log);
        } else {
            return PomHelper.getChildModels(project, log).values().stream()
                    .map(Model::getBuild)
                    .filter(Objects::nonNull)
                    .map(Build::getExtensions)
                    .map(List::stream)
                    .reduce(Stream::concat)
                    .orElse(Stream.empty());
        }
    }

    private static Stream<Extension> getInterpolatedBuildExtensions(MavenProject project, Log log)
            throws IOException, XMLStreamException, TransformerException {
        MutableXMLStreamReader pomReader =
                new MutableXMLStreamReader(project.getFile().toPath());
        ModelNode rootNode = new ModelNode(PomHelper.getRawModel(pomReader.getSource(), project.getFile()), pomReader);
        List<ModelNode> rawModels = PomHelper.getRawModelTree(rootNode, log);
        return rawModels.stream()
                .filter(node -> Objects.nonNull(node.getModel()))
                .filter(node -> ofNullable(node.getModel().getBuild())
                        .map(Build::getExtensions)
                        .map(list -> !list.isEmpty())
                        .orElse(false))
                .map(node -> Pair.of(node.getModel().getBuild().getExtensions(), getNodeProperties(node)))
                .flatMap(pair -> pair.getLeft().stream().map(e -> Pair.of(e, pair.getRight())))
                .map(pair -> ExtensionBuilder.newBuilder()
                        .withGroupId(PomHelper.evaluate(pair.getLeft().getGroupId(), pair.getRight(), log))
                        .withArtifactId(PomHelper.evaluate(pair.getLeft().getArtifactId(), pair.getRight(), log))
                        .withVersion(PomHelper.evaluate(pair.getLeft().getVersion(), pair.getRight(), log))
                        .build());
    }

    private static Map<String, String> getNodeProperties(ModelNode node) {
        Map<String, String> properties = new HashMap<>();
        for (ModelNode p = node; p != null; p = p.getParent().orElse(null)) {
            p.getModel()
                    .getProperties()
                    .forEach((key, value) -> properties.putIfAbsent(String.valueOf(key), String.valueOf(value)));
        }
        return properties;
    }
}
