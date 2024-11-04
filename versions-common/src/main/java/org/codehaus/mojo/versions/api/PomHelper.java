package org.codehaus.mojo.versions.api;

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

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.TransformerException;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.model.building.ModelBuildingRequest;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.DefaultProjectBuildingRequest;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.apache.maven.shared.utils.io.IOUtil;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ModelNode;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.codehaus.stax2.XMLInputFactory2;

import static java.util.Collections.singletonMap;
import static java.util.Optional.ofNullable;
import static org.codehaus.mojo.versions.api.PomHelper.Marks.CHILD_START;
import static org.codehaus.mojo.versions.api.PomHelper.Marks.END_ELEMENT;
import static org.codehaus.mojo.versions.api.PomHelper.Marks.PARENT_START;

/**
 * Helper class for modifying pom files.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public final class PomHelper {
    public static final String APACHE_MAVEN_PLUGINS_GROUPID = "org.apache.maven.plugins";

    public static final Pattern PATTERN_PROJECT_PROPERTIES = Pattern.compile("/project/properties");

    public static final Pattern PATTERN_PROJECT_PROFILE = Pattern.compile("/project/profiles/profile");

    public static final Pattern PATTERN_PROJECT_PROFILE_ID = Pattern.compile("/project/profiles/profile/id");

    public static final Pattern PATTERN_PROJECT_VERSION = Pattern.compile("/project/version");

    public static final Pattern PATTERN_PROJECT_PARENT_VERSION = Pattern.compile("/project/parent/version");

    public static final Pattern PATTERN_PROJECT_DEPENDENCY = Pattern.compile("/project" + "(/profiles/profile)?"
            + "((/dependencyManagement)|(/build(/pluginManagement)?/plugins/plugin))?"
            + "/dependencies/dependency");

    public static final Pattern PATTERN_PROJECT_DEPENDENCY_VERSION = Pattern.compile("/project" + "(/profiles/profile)?"
            + "((/dependencyManagement)|(/build(/pluginManagement)?/plugins/plugin))?"
            + "/dependencies/dependency"
            + "((/groupId)|(/artifactId)|(/version))");

    public static final Pattern PATTERN_PROJECT_PLUGIN = Pattern.compile(
            "/project" + "(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))/plugins/plugin");

    public static final Pattern PATTERN_PROJECT_PLUGIN_VERSION = Pattern.compile("/project" + "(/profiles/profile)?"
            + "((/build(/pluginManagement)?)|(/reporting))/plugins/plugin"
            + "((/groupId)|(/artifactId)|(/version))");

    private PomHelper() {
        // utility class
    }

    /**
     * Gets the raw model before any interpolation what-so-ever.
     *
     * @param project The project to getModel the raw model for.
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel(MavenProject project) throws IOException {
        return getRawModel(project.getFile());
    }

    /**
     * Gets the raw model before any interpolation what-so-ever.
     *
     * @param moduleProjectFile The project file to getModel the raw model for.
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel(File moduleProjectFile) throws IOException {
        try (Reader reader =
                new BufferedReader(new InputStreamReader(Files.newInputStream(moduleProjectFile.toPath())))) {
            Model result = getRawModel(reader);
            result.setPomFile(moduleProjectFile);
            return result;
        }
    }

    /**
     * Gets the current raw model before any interpolation what-so-ever.
     *
     * @param modelString a string containing the raw model
     * @param modelPath the File containing the model
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel(String modelString, File modelPath) throws IOException {
        try (Reader reader = new StringReader(modelString)) {
            Model result = getRawModel(reader);
            result.setPomFile(modelPath);
            return result;
        }
    }

    /**
     * Gets the current raw model before any interpolation what-so-ever.
     *
     * @param reader The {@link Reader} to getModel the raw model for.
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel(Reader reader) throws IOException {
        try {
            return new MavenXpp3Reader().read(reader);
        } catch (XmlPullParserException e) {
            throw new IOException(e.getMessage(), e);
        }
    }

    /**
     * Searches the pom re-defining the specified property to the specified version.
     *
     * @param pom       The pom to modify.
     * @param profileId The profile in which to modify the property.
     * @param property  The property to modify.
     * @param value     The new value of the property.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setPropertyVersion(
            final MutableXMLStreamReader pom, final String profileId, final String property, final String value)
            throws XMLStreamException {
        Stack<String> stack = new Stack<>();
        String path = "";
        final Pattern propertyRegex;
        final Pattern matchScopeRegex;
        final Pattern projectProfileId;
        boolean inMatchScope = false;
        boolean madeReplacement = false;
        if (profileId == null) {
            propertyRegex = Pattern.compile("/project/properties/" + RegexUtils.quote(property));
            matchScopeRegex = PATTERN_PROJECT_PROPERTIES;
            projectProfileId = null;
        } else {
            propertyRegex = Pattern.compile("/project/profiles/profile/properties/" + RegexUtils.quote(property));
            matchScopeRegex = PATTERN_PROJECT_PROFILE;
            projectProfileId = PATTERN_PROJECT_PROFILE_ID;
        }

        pom.rewind();
        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                stack.push(path);
                path = path + "/" + pom.getLocalName();

                if (propertyRegex.matcher(path).matches()) {
                    pom.mark(0);
                } else if (matchScopeRegex.matcher(path).matches()) {
                    // we're in a new match scope
                    // reset any previous partial matches
                    inMatchScope = profileId == null;
                    pom.clearMark(0);
                    pom.clearMark(1);
                } else if (profileId != null && projectProfileId.matcher(path).matches()) {
                    String candidateId = pom.getElementText();

                    inMatchScope = profileId.trim().equals(candidateId.trim());
                }
            }
            // for empty elements, pom can be both start- and end element
            if (pom.isEndElement()) {
                if (propertyRegex.matcher(path).matches()) {
                    pom.mark(1);
                } else if (matchScopeRegex.matcher(path).matches()) {
                    if (inMatchScope && pom.hasMark(0) && pom.hasMark(1)) {
                        pom.replaceBetween(0, 1, value);
                        madeReplacement = true;
                    }
                    pom.clearMark(0);
                    pom.clearMark(1);
                    inMatchScope = false;
                }
                path = stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Searches the pom re-defining the project version to the specified version.
     *
     * @param pom   The pom to modify.
     * @param value The new value of the property.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setProjectVersion(final MutableXMLStreamReader pom, final String value)
            throws XMLStreamException {
        return setElementValue(pom, "/project", "version", value, false);
    }

    /**
     * Sets the value of the given element given its parent element path.
     * Will only consider the first found occurrence of the parent element.
     * If the element is not found in the parent element, the method will create the element.
     *
     * @param pom         pom to modify
     * @param parentPath  path of the parent element
     * @param elementName name of the element to set or create
     * @param value       the new value of the element
     * @return {@code true} if the element was created or replaced
     * @throws XMLStreamException if something went wrong
     */
    public static boolean setElementValue(
            MutableXMLStreamReader pom, String parentPath, String elementName, String value) throws XMLStreamException {
        pom.rewind();
        return setElementValue(pom, parentPath, elementName, value, true);
    }

    enum Marks {
        CHILD_START,
        PARENT_START,
        END_ELEMENT
    }

    /**
     * Sets the value of the given element given its parent element path.
     * Will only consider the first found occurrence of the parent element.
     * If the element is not found in the parent element, the method will create the element
     * if {@code shouldCreate} is {@code true}.
     *
     * @param pom          pom to modify
     * @param parentPath   path of the parent element
     * @param elementName  name of the element to set or create
     * @param value        the new value of the element
     * @param shouldCreate should the element be created if it's not found in the first encountered parent element
     *                     matching the parentPath
     * @return {@code true} if the element was created or replaced
     * @throws XMLStreamException if something went wrong
     */
    public static boolean setElementValue(
            MutableXMLStreamReader pom, String parentPath, String elementName, String value, boolean shouldCreate)
            throws XMLStreamException {
        class ElementValueInternal {
            private final String parentName;
            private final String superParentPath;

            ElementValueInternal() {
                int lastDelimeterIndex = parentPath.lastIndexOf('/');
                parentName = parentPath.substring(lastDelimeterIndex + 1);
                superParentPath = parentPath.substring(0, lastDelimeterIndex);
            }

            boolean process(String currentPath) throws XMLStreamException {
                boolean replacementMade = false;
                while (!replacementMade && pom.hasNext()) {
                    pom.next();
                    if (pom.isStartElement()) {
                        // here, we will only mark the beginning of the child or the parent element
                        if (currentPath.equals(parentPath) && elementName.equals(pom.getLocalName())) {
                            pom.mark(CHILD_START);
                        } else if (currentPath.equals(superParentPath)
                                && pom.getLocalName().equals(parentName)) {
                            pom.mark(PARENT_START);
                        }
                        // process child element
                        replacementMade = process(currentPath + "/" + pom.getLocalName());
                    } else if (pom.isEndElement()) {
                        // here we're doing the replacement
                        if (currentPath.equals(parentPath + "/" + elementName)) {
                            // end of the child
                            replaceValueInChild();
                            replacementMade = true;
                        } else if (shouldCreate && currentPath.equals(parentPath)) {
                            // end of the parent
                            replaceValueInParent();
                            replacementMade = true;
                        } else {
                            return false;
                        }
                    }
                }
                return replacementMade;
            }

            private void replaceValueInChild() {
                pom.mark(END_ELEMENT);
                if (!pom.getBetween(CHILD_START, END_ELEMENT).isEmpty()) {
                    pom.replaceBetween(CHILD_START, END_ELEMENT, value);
                } else {
                    pom.replace(String.format("<%1$s>%2$s</%1$s>", elementName, value));
                }
            }

            private void replaceValueInParent() {
                pom.mark(END_ELEMENT);
                if (pom.hasMark(PARENT_START)) {
                    if (!pom.getBetween(PARENT_START, END_ELEMENT).isEmpty()) {
                        pom.replace(String.format("<%2$s>%3$s</%2$s></%1$s>", parentName, elementName, value));
                    } else {
                        pom.replace(String.format("<%1$s><%2$s>%3$s</%2$s></%1$s>", parentName, elementName, value));
                    }
                } else {
                    pom.replace(String.format("<%1$s><%2$s>%3$s</%2$s></%1$s>", parentName, elementName, value));
                }
            }
        }

        pom.rewind();
        return new ElementValueInternal().process("");
    }

    /**
     * Retrieves the project version from the pom.
     *
     * @param pom The pom.
     * @return the project version or <code>null</code> if the project version is not defined (i.e. inherited from
     * parent version).
     * @throws XMLStreamException if something went wrong.
     */
    public static String getProjectVersion(final MutableXMLStreamReader pom) throws XMLStreamException {
        Stack<String> stack = new Stack<>();
        String path = "";

        pom.rewind();
        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                stack.push(path);
                path = path + "/" + pom.getLocalName();

                if (PATTERN_PROJECT_VERSION.matcher(path).matches()) {
                    pom.mark(0);
                }
            }
            // for empty elements, pom can be both start- and end element
            if (pom.isEndElement()) {
                if (PATTERN_PROJECT_VERSION.matcher(path).matches()) {
                    pom.mark(1);
                    if (pom.hasMark(0) && pom.hasMark(1)) {
                        return pom.getBetween(0, 1).trim();
                    }
                    pom.clearMark(0);
                    pom.clearMark(1);
                }
                path = stack.pop();
            }
        }
        return null;
    }

    /**
     * Searches the pom re-defining the project version to the specified version.
     *
     * @param pom   The pom to modify.
     * @param value The new value of the property.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setProjectParentVersion(final MutableXMLStreamReader pom, final String value)
            throws XMLStreamException {
        Stack<String> stack = new Stack<>();
        String path = "";
        boolean madeReplacement = false;

        pom.rewind();
        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                stack.push(path);
                path = path + "/" + pom.getLocalName();

                if (PATTERN_PROJECT_PARENT_VERSION.matcher(path).matches()) {
                    pom.mark(0);
                }
            }
            // for empty elements, pom can be both start- and end element
            if (pom.isEndElement()) {
                if (PATTERN_PROJECT_PARENT_VERSION.matcher(path).matches()) {
                    pom.mark(1);
                    if (pom.hasMark(0) && pom.hasMark(1)) {
                        pom.replaceBetween(0, 1, value);
                        madeReplacement = true;
                    }
                    pom.clearMark(0);
                    pom.clearMark(1);
                }
                path = stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Searches the pom re-defining the specified dependency to the specified version.
     *
     * @param pom        The pom to modify.
     * @param groupId    The groupId of the dependency.
     * @param artifactId The artifactId of the dependency.
     * @param oldVersion The old version of the dependency.
     * @param newVersion The new version of the dependency.
     * @param model      The model to getModel the project properties from.
     * @param logger     The logger to use.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if something went wrong.
     */
    @SuppressWarnings("checkstyle:MethodLength")
    public static boolean setDependencyVersion(
            final MutableXMLStreamReader pom,
            final String groupId,
            final String artifactId,
            final String oldVersion,
            final String newVersion,
            final Model model,
            final Log logger)
            throws XMLStreamException {
        Stack<String> stack = new Stack<>();
        String path = "";

        Set<String> implicitPaths = new HashSet<>(Arrays.asList(
                "/project/parent/groupId", "/project/parent/artifactId",
                "/project/parent/version", "/project/groupId",
                "/project/artifactId", "/project/version"));
        Map<String, String> implicitProperties = new HashMap<>();

        for (Map.Entry<Object, Object> entry : model.getProperties().entrySet()) {
            implicitProperties.put((String) entry.getKey(), (String) entry.getValue());
        }

        pom.rewind();
        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                stack.push(path);
                path = path + "/" + pom.getLocalName();

                if (implicitPaths.contains(path)) {
                    final String elementText = pom.getElementText().trim();
                    implicitProperties.put(path.substring(1).replace('/', '.'), elementText);
                }
            }
            if (pom.isEndElement()) {
                path = stack.pop();
            }
        }

        for (boolean modified = true; modified; ) {
            modified = false;
            for (Map.Entry<String, String> entry : implicitProperties.entrySet()) {
                if (entry.getKey().contains(".parent")) {
                    String child = entry.getKey().replace(".parent", "");
                    if (!implicitProperties.containsKey(child)) {
                        implicitProperties.put(child, entry.getValue());
                        modified = true;
                        break;
                    }
                }
            }
        }

        stack = new Stack<>();
        path = "";
        boolean inMatchScope = false;
        boolean madeReplacement = false;
        boolean haveGroupId = false;
        boolean haveArtifactId = false;
        boolean haveOldVersion = false;

        pom.rewind();
        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                stack.push(path);
                path = path + "/" + pom.getLocalName();

                if (PATTERN_PROJECT_DEPENDENCY.matcher(path).matches()) {
                    // we're in a new match scope
                    // reset any previous partial matches
                    inMatchScope = true;
                    pom.clearMark(0);
                    pom.clearMark(1);

                    haveGroupId = false;
                    haveArtifactId = false;
                    haveOldVersion = false;
                } else if (inMatchScope
                        && PATTERN_PROJECT_DEPENDENCY_VERSION.matcher(path).matches()) {
                    if ("groupId".equals(pom.getLocalName())) {
                        haveGroupId =
                                groupId.equals(evaluate(pom.getElementText().trim(), implicitProperties, logger));
                    } else if ("artifactId".equals(pom.getLocalName())) {
                        haveArtifactId =
                                artifactId.equals(evaluate(pom.getElementText().trim(), implicitProperties, logger));
                    } else if ("version".equals(pom.getLocalName())) {
                        pom.mark(0);
                    }
                }
            }
            // for empty elements, pom can be both start- and end element
            if (pom.isEndElement()) {
                if (PATTERN_PROJECT_DEPENDENCY_VERSION.matcher(path).matches()
                        && "version".equals(pom.getLocalName())) {
                    pom.mark(1);
                    String compressedPomVersion =
                            StringUtils.deleteWhitespace(pom.getBetween(0, 1).trim());
                    String compressedOldVersion = StringUtils.deleteWhitespace(oldVersion);

                    try {
                        haveOldVersion = isVersionOverlap(compressedOldVersion, compressedPomVersion);
                    } catch (InvalidVersionSpecificationException e) {
                        // fall back to string comparison
                        haveOldVersion = compressedOldVersion.equals(compressedPomVersion);
                    }
                } else if (PATTERN_PROJECT_DEPENDENCY.matcher(path).matches()) {
                    if (inMatchScope
                            && pom.hasMark(0)
                            && pom.hasMark(1)
                            && haveGroupId
                            && haveArtifactId
                            && haveOldVersion) {
                        pom.replaceBetween(0, 1, newVersion);
                        madeReplacement = true;
                    }
                    pom.clearMark(0);
                    pom.clearMark(1);
                    haveArtifactId = false;
                    haveGroupId = false;
                    haveOldVersion = false;
                    inMatchScope = false;
                }
                path = stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * A lightweight expression evaluation function.
     *
     * @param expr       The expression to evaluate.
     * @param properties The properties to substitute.
     * @param logger     The logger to use.
     * @return The evaluated expression.
     */
    public static String evaluate(String expr, Map<String, String> properties, Log logger) {
        if (expr == null) {
            return null;
        }

        return extractExpression(expr)
                .map(expression -> {
                    String value = properties.get(expression);

                    if (value != null) {
                        int exprStartDelimiter = value.indexOf("${");

                        if (exprStartDelimiter >= 0) {
                            if (exprStartDelimiter > 0) {
                                value = value.substring(0, exprStartDelimiter)
                                        + evaluate(value.substring(exprStartDelimiter), properties, logger);
                            } else {
                                value = evaluate(value.substring(exprStartDelimiter), properties, logger);
                            }
                        }
                    } else {
                        // Because we work with the raw model, without interpolation, unevaluatable expressions are not
                        // unexpected
                        logger.debug("expression: " + expression + " no value ");
                    }
                    return value == null ? expr : value;
                })
                .orElseGet(() -> {
                    int index = expr.indexOf("${");
                    if (index >= 0) {
                        int lastIndex = expr.indexOf("}", index);
                        if (lastIndex >= 0) {
                            String retVal = expr.substring(0, index);

                            if (index > 0 && expr.charAt(index - 1) == '$') {
                                retVal += expr.substring(index + 1, lastIndex + 1);
                            } else {
                                retVal += evaluate(expr.substring(index, lastIndex + 1), properties, logger);
                            }

                            retVal += evaluate(expr.substring(lastIndex + 1), properties, logger);
                            return retVal;
                        }
                    }

                    // Was not an expression
                    if (expr.contains("$$")) {
                        return expr.replaceAll("\\$\\$", "\\$");
                    } else {
                        return expr;
                    }
                });
    }

    /**
     * Strips the expression token markers from the start and end of the string.
     *
     * @param expr the string (perhaps with token markers)
     * @return the string (without token markers) if a property has been found, {@link Optional#empty()}
     * otherwise
     */
    public static Optional<String> extractExpression(String expr) {
        return ofNullable(expr)
                .map(String::trim)
                .filter(e -> e.startsWith("${") && e.indexOf("}") == e.length() - 1)
                .map(e -> e.substring(2, e.length() - 1));
    }

    /**
     * Checks if two versions or ranges have an overlap.
     *
     * @param leftVersionOrRange  the 1st version number or range to test
     * @param rightVersionOrRange the 2nd version number or range to test
     * @return true if both versions have an overlap
     * @throws InvalidVersionSpecificationException if the versions can't be parsed to a range
     */
    public static boolean isVersionOverlap(String leftVersionOrRange, String rightVersionOrRange)
            throws InvalidVersionSpecificationException {
        VersionRange pomVersionRange = createVersionRange(leftVersionOrRange);
        if (!pomVersionRange.hasRestrictions()) {
            return true;
        }

        VersionRange oldVersionRange = createVersionRange(rightVersionOrRange);
        if (!oldVersionRange.hasRestrictions()) {
            return true;
        }

        VersionRange result = oldVersionRange.restrict(pomVersionRange);
        return result.hasRestrictions();
    }

    private static VersionRange createVersionRange(String versionOrRange) throws InvalidVersionSpecificationException {
        VersionRange versionRange = VersionRange.createFromVersionSpec(versionOrRange);
        if (versionRange.getRecommendedVersion() != null) {
            versionRange = VersionRange.createFromVersionSpec("[" + versionOrRange + "]");
        }
        return versionRange;
    }

    /**
     * Searches the pom re-defining the specified plugin to the specified version.
     *
     * @param pom        The pom to modify.
     * @param groupId    The groupId of the dependency.
     * @param artifactId The artifactId of the dependency.
     * @param oldVersion The old version of the dependency.
     * @param newVersion The new version of the dependency.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setPluginVersion(
            final MutableXMLStreamReader pom,
            final String groupId,
            final String artifactId,
            final String oldVersion,
            final String newVersion)
            throws XMLStreamException {
        Stack<String> stack = new Stack<>();
        String path = "";
        boolean inMatchScope = false;
        boolean madeReplacement = false;
        boolean haveGroupId = false;
        boolean needGroupId = groupId != null && !APACHE_MAVEN_PLUGINS_GROUPID.equals(groupId);
        boolean haveArtifactId = false;
        boolean haveOldVersion = false;

        pom.rewind();
        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                stack.push(path);
                final String elementName = pom.getLocalName();
                path = path + "/" + elementName;

                if (PATTERN_PROJECT_PLUGIN.matcher(path).matches()) {
                    // we're in a new match scope
                    // reset any previous partial matches
                    inMatchScope = true;
                    pom.clearMark(0);
                    pom.clearMark(1);

                    haveGroupId = false;
                    haveArtifactId = false;
                    haveOldVersion = false;
                } else if (inMatchScope
                        && PATTERN_PROJECT_PLUGIN_VERSION.matcher(path).matches()) {
                    if ("groupId".equals(elementName)) {
                        haveGroupId = pom.getElementText().trim().equals(groupId);
                    } else if ("artifactId".equals(elementName)) {
                        haveArtifactId = artifactId.equals(pom.getElementText().trim());
                    } else if ("version".equals(elementName)) {
                        pom.mark(0);
                    }
                }
            }
            // for empty elements, pom can be both start- and end element
            if (pom.isEndElement()) {
                if (PATTERN_PROJECT_PLUGIN_VERSION.matcher(path).matches() && "version".equals(pom.getLocalName())) {
                    pom.mark(1);

                    try {
                        haveOldVersion = isVersionOverlap(
                                oldVersion, pom.getBetween(0, 1).trim());
                    } catch (InvalidVersionSpecificationException e) {
                        // fall back to string comparison
                        haveOldVersion = oldVersion.equals(pom.getBetween(0, 1).trim());
                    }
                } else if (PATTERN_PROJECT_PLUGIN.matcher(path).matches()) {
                    if (inMatchScope
                            && pom.hasMark(0)
                            && pom.hasMark(1)
                            && (haveGroupId || !needGroupId)
                            && haveArtifactId
                            && haveOldVersion) {
                        pom.replaceBetween(0, 1, newVersion);
                        madeReplacement = true;
                        pom.clearMark(0);
                        pom.clearMark(1);
                        haveArtifactId = false;
                        haveGroupId = false;
                        haveOldVersion = false;
                    }
                    inMatchScope = false;
                }
                path = stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Traverses the project tree upwards, adding the raw models of every project it encounters to the map
     *
     * @param project maven project of the child for which the models need to be gathered
     * @return gathered map of raw models per project
     */
    private static Map<MavenProject, Model> getRawModelWithParents(MavenProject project) throws IOException {
        // constructs a tree sorted from children to parents
        Map<MavenProject, Model> models = new TreeMap<>((p1, p2) -> {
            for (MavenProject p = p1; p != null; p = p.getParent()) {
                if (p == p2) // meaning p2 is an ancestor to p1 or p1 == p2
                {
                    return p == p1 ? 0 : -1; // p1 is the child
                }
            }
            return 1;
        });
        for (MavenProject p = project; p != null; p = p.getParent()) {
            models.put(p, p.getFile() != null ? getRawModel(p) : p.getOriginalModel());
        }
        return models;
    }

    /**
     * Examines the project to find any properties which are associated with versions of artifacts in the project.
     *
     * @param helper        Our versions helper.
     * @param project       The project to examine.
     * @param includeParent whether parent POMs should be included
     * @return An array of properties that are associated within the project.
     * @throws ExpressionEvaluationException if an expression cannot be evaluated.
     * @throws IOException                   if the project's pom file cannot be parsed.
     * @since 1.0-alpha-3
     */
    public static PropertyVersionsBuilder[] getPropertyVersionsBuilders(
            VersionsHelper helper, MavenProject project, boolean includeParent)
            throws ExpressionEvaluationException, IOException {
        ExpressionEvaluator expressionEvaluator = helper.getExpressionEvaluator(project);
        Map<MavenProject, Model> reactorModels =
                includeParent ? getRawModelWithParents(project) : singletonMap(project, getRawModel(project));

        Map<String, PropertyVersionsBuilder> propertiesMap = new TreeMap<>();

        Set<String> activeProfiles = new TreeSet<>();
        for (Profile profile : project.getActiveProfiles()) {
            activeProfiles.add(profile.getId());
        }

        // add any properties from profiles first (as they override properties from the project
        for (Iterator<Profile> it = reactorModels.values().stream()
                        .flatMap(model -> model.getProfiles().stream())
                        .filter(profile -> activeProfiles.contains(profile.getId()))
                        .iterator();
                it.hasNext(); ) {
            Profile profile = it.next();
            try {
                addProperties(helper, propertiesMap, profile.getId(), profile.getProperties());
                if (profile.getDependencyManagement() != null) {
                    addDependencyAssocations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            profile.getDependencyManagement().getDependencies(),
                            false);
                }
                addDependencyAssocations(helper, expressionEvaluator, propertiesMap, profile.getDependencies(), false);
                if (profile.getBuild() != null) {
                    if (profile.getBuild().getPluginManagement() != null) {
                        addPluginAssociations(
                                helper,
                                expressionEvaluator,
                                propertiesMap,
                                profile.getBuild().getPluginManagement().getPlugins());
                    }
                    addPluginAssociations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            profile.getBuild().getPlugins());
                }
                if (profile.getReporting() != null) {
                    addReportPluginAssociations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            profile.getReporting().getPlugins());
                }
            } catch (ExpressionEvaluationException e) {
                throw new RuntimeException(e);
            }
        }

        // second, we add all the properties in the pom
        reactorModels.values().forEach(model -> addProperties(helper, propertiesMap, null, model.getProperties()));

        for (MavenProject currentPrj = project;
                currentPrj != null;
                currentPrj = includeParent ? currentPrj.getParent() : null) {
            Model model = reactorModels.get(currentPrj);

            if (model.getDependencyManagement() != null) {
                addDependencyAssocations(
                        helper,
                        expressionEvaluator,
                        propertiesMap,
                        model.getDependencyManagement().getDependencies(),
                        false);
            }
            addDependencyAssocations(helper, expressionEvaluator, propertiesMap, model.getDependencies(), false);
            if (model.getBuild() != null) {
                if (model.getBuild().getPluginManagement() != null) {
                    addPluginAssociations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            model.getBuild().getPluginManagement().getPlugins());
                }
                addPluginAssociations(
                        helper,
                        expressionEvaluator,
                        propertiesMap,
                        model.getBuild().getPlugins());
            }
            if (model.getReporting() != null) {
                addReportPluginAssociations(
                        helper,
                        expressionEvaluator,
                        propertiesMap,
                        model.getReporting().getPlugins());
            }

            // third, we add any associations from the active profiles
            for (Profile profile : model.getProfiles()) {
                if (!activeProfiles.contains(profile.getId())) {
                    continue;
                }
                if (profile.getDependencyManagement() != null) {
                    addDependencyAssocations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            profile.getDependencyManagement().getDependencies(),
                            false);
                }
                addDependencyAssocations(helper, expressionEvaluator, propertiesMap, profile.getDependencies(), false);
                if (profile.getBuild() != null) {
                    if (profile.getBuild().getPluginManagement() != null) {
                        addPluginAssociations(
                                helper,
                                expressionEvaluator,
                                propertiesMap,
                                profile.getBuild().getPluginManagement().getPlugins());
                    }
                    addPluginAssociations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            profile.getBuild().getPlugins());
                }
                if (profile.getReporting() != null) {
                    addReportPluginAssociations(
                            helper,
                            expressionEvaluator,
                            propertiesMap,
                            profile.getReporting().getPlugins());
                }
            }
        }

        // finally, remove any properties without associations
        purgeProperties(propertiesMap);

        return propertiesMap.values().toArray(new PropertyVersionsBuilder[0]);
    }

    /**
     * Takes a list of {@link org.apache.maven.model.Plugin} instances and adds associations to properties used to
     * define versions of the plugin artifact or any of the plugin dependencies specified in the pom.
     *
     * @param helper              Our helper.
     * @param expressionEvaluator Our expression evaluator.
     * @param result              The map of {@link org.codehaus.mojo.versions.api.PropertyVersionsBuilder} keyed by
     *                            property name.
     * @param plugins             The list of {@link org.apache.maven.model.Plugin}.
     * @throws ExpressionEvaluationException if an expression cannot be evaluated.
     */
    private static void addPluginAssociations(
            VersionsHelper helper,
            ExpressionEvaluator expressionEvaluator,
            Map<String, PropertyVersionsBuilder> result,
            List<Plugin> plugins)
            throws ExpressionEvaluationException {
        if (plugins == null) {
            return;
        }
        for (Plugin plugin : plugins) {
            String version = plugin.getVersion();
            if (version != null && version.contains("${") && version.indexOf('}') != -1) {
                version = StringUtils.deleteWhitespace(version);
                for (PropertyVersionsBuilder property : result.values()) {
                    // any of these could be defined by a property
                    final String propertyRef = "${" + property.getName() + "}";
                    if (version.contains(propertyRef)) {
                        String groupId = plugin.getGroupId();
                        if (StringUtils.isBlank(groupId)) {
                            // groupId has a special default
                            groupId = APACHE_MAVEN_PLUGINS_GROUPID;
                        } else {
                            groupId = (String) expressionEvaluator.evaluate(groupId);
                        }
                        String artifactId = plugin.getArtifactId();
                        if (StringUtils.isBlank(artifactId)) {
                            // malformed pom
                            continue;
                        } else {
                            artifactId = (String) expressionEvaluator.evaluate(artifactId);
                        }
                        // might as well capture the current value
                        String evaluatedVersion = (String) expressionEvaluator.evaluate(plugin.getVersion());
                        property.withAssociation(
                                helper.createPluginArtifact(groupId, artifactId, evaluatedVersion), true);
                        if (!propertyRef.equals(version)) {
                            addBounds(property, version, propertyRef);
                        }
                    }
                }
            }
            addDependencyAssocations(helper, expressionEvaluator, result, plugin.getDependencies(), true);
        }
    }

    private static void addReportPluginAssociations(
            VersionsHelper helper,
            ExpressionEvaluator expressionEvaluator,
            Map<String, PropertyVersionsBuilder> result,
            List<ReportPlugin> reportPlugins)
            throws ExpressionEvaluationException {
        if (reportPlugins == null) {
            return;
        }
        for (ReportPlugin plugin : reportPlugins) {
            String version = plugin.getVersion();
            if (version != null && version.contains("${") && version.indexOf('}') != -1) {
                version = StringUtils.deleteWhitespace(version);
                for (PropertyVersionsBuilder property : result.values()) {
                    final String propertyRef = "${" + property.getName() + "}";
                    if (version.contains(propertyRef)) {
                        // any of these could be defined by a property
                        String groupId = plugin.getGroupId();
                        if (StringUtils.isBlank(groupId)) {
                            // groupId has a special default
                            groupId = APACHE_MAVEN_PLUGINS_GROUPID;
                        } else {
                            groupId = (String) expressionEvaluator.evaluate(groupId);
                        }
                        String artifactId = plugin.getArtifactId();
                        if (StringUtils.isBlank(artifactId)) {
                            // malformed pom
                            continue;
                        } else {
                            artifactId = (String) expressionEvaluator.evaluate(artifactId);
                        }
                        // might as well capture the current value
                        String versionEvaluated = (String) expressionEvaluator.evaluate(plugin.getVersion());
                        property.withAssociation(
                                helper.createPluginArtifact(groupId, artifactId, versionEvaluated), true);
                        if (!propertyRef.equals(version)) {
                            addBounds(property, version, propertyRef);
                        }
                    }
                }
            }
        }
    }

    private static void addDependencyAssocations(
            VersionsHelper helper,
            ExpressionEvaluator expressionEvaluator,
            Map<String, PropertyVersionsBuilder> result,
            List<Dependency> dependencies,
            boolean usePluginRepositories)
            throws ExpressionEvaluationException {
        if (dependencies == null) {
            return;
        }
        for (Dependency dependency : dependencies) {
            String version = dependency.getVersion();
            if (version != null && version.contains("${") && version.indexOf('}') != -1) {
                version = StringUtils.deleteWhitespace(version);
                for (PropertyVersionsBuilder property : result.values()) {
                    final String propertyRef = "${" + property.getName() + "}";
                    if (version.contains(propertyRef)) {
                        // Any of these could be defined by a property
                        String groupId = dependency.getGroupId();
                        if (StringUtils.isBlank(groupId)) {
                            // malformed pom
                            continue;
                        } else {
                            groupId = (String) expressionEvaluator.evaluate(groupId);
                        }
                        String artifactId = dependency.getArtifactId();
                        if (StringUtils.isBlank(artifactId)) {
                            // malformed pom
                            continue;
                        } else {
                            artifactId = (String) expressionEvaluator.evaluate(artifactId);
                        }
                        // might as well capture the current value
                        String versionEvaluated = (String) expressionEvaluator.evaluate(dependency.getVersion());
                        property.withAssociation(
                                helper.createDependencyArtifact(
                                        groupId,
                                        artifactId,
                                        versionEvaluated,
                                        dependency.getType(),
                                        dependency.getClassifier(),
                                        dependency.getScope(),
                                        dependency.isOptional()),
                                usePluginRepositories);
                        if (!propertyRef.equals(version)) {
                            addBounds(property, version, propertyRef);
                        }
                    }
                }
            }
        }
    }

    private static void addBounds(PropertyVersionsBuilder builder, String rawVersionRange, String propertyRef) {
        Pattern lowerBound = Pattern.compile("([(\\[])([^,]*)," + RegexUtils.quote(propertyRef) + "([)\\]])");
        Pattern upperBound = Pattern.compile("([(\\[])" + RegexUtils.quote(propertyRef) + ",([^,]*)([)\\]])");
        Matcher m = lowerBound.matcher(rawVersionRange);
        if (m.find()) {
            boolean includeLower = "[".equals(m.group(1));
            String lowerLimit = m.group(2);
            if (StringUtils.isNotEmpty(lowerLimit)) {
                builder.withLowerBound(lowerLimit, includeLower);
            }
        }
        m = upperBound.matcher(rawVersionRange);
        if (m.find()) {
            boolean includeUpper = "[".equals(m.group(3));
            String upperLimit = m.group(2);
            if (StringUtils.isNotEmpty(upperLimit)) {
                builder.withUpperBound(upperLimit, includeUpper);
            }
        }
    }

    private static void addProperties(
            VersionsHelper helper,
            Map<String, PropertyVersionsBuilder> result,
            String profileId,
            Properties properties) {
        if (properties == null) {
            return;
        }
        for (String propertyName : properties.stringPropertyNames()) {
            if (!result.containsKey(propertyName)) {
                result.put(propertyName, new PropertyVersionsBuilder(profileId, propertyName, helper));
            }
        }
    }

    private static void purgeProperties(Map<String, PropertyVersionsBuilder> result) {
        result.values().removeIf(versions -> versions.getAssociations().length == 0);
    }

    /**
     * Returns a set of all child modules for a project, including any defined in profiles (ignoring profile
     * activation).
     *
     * @param project The project.
     * @param logger  The logger to use.
     * @return the set of all child modules of the project.
     */
    public static Set<String> getAllChildModules(MavenProject project, Log logger) {
        return getAllChildModules(project.getOriginalModel(), logger);
    }

    /**
     * Returns a set of all child modules for a project, including any defined in profiles (ignoring profile
     * activation).
     *
     * @param model  The project model.
     * @param logger The logger to use.
     * @return the set of all child modules of the project.
     */
    public static Set<String> getAllChildModules(Model model, Log logger) {
        logger.debug("Finding child modules of " + model);
        Set<String> childModules = new TreeSet<>(model.getModules());
        model.getProfiles().forEach(profile -> childModules.addAll(profile.getModules()));
        debugModules(logger, "Child modules:", childModules);
        return childModules;
    }

    /**
     * Outputs a debug message with a list of modules.
     *
     * @param logger  The logger to log to.
     * @param message The message to display.
     * @param modules The modules to append to the message.
     */
    public static void debugModules(Log logger, String message, Collection<String> modules) {
        if (logger.isDebugEnabled()) {
            logger.debug(message);
            if (modules.isEmpty()) {
                logger.debug("None.");
            } else {
                modules.forEach(module -> logger.debug("  " + module));
            }
        }
    }

    /**
     * Extracts the version from a raw model, interpolating from the parent if necessary.
     *
     * @param model The model.
     * @return The version.
     */
    public static String getVersion(Model model) {
        String targetVersion = model.getVersion();
        if (targetVersion == null && model.getParent() != null) {
            targetVersion = model.getParent().getVersion();
        }
        return targetVersion;
    }

    /**
     * Checks to see if the model contains an explicitly specified version.
     *
     * @param model The model.
     * @return {@code true} if the model explicitly specifies the project version, i.e. /project/version
     */
    public static boolean isExplicitVersion(Model model) {
        return model.getVersion() != null;
    }

    /**
     * Extracts the artifactId from a raw model, interpolating from the parent if necessary.
     *
     * @param model The model.
     * @return The artifactId.
     */
    public static String getArtifactId(Model model) {
        String sourceArtifactId = model.getArtifactId();
        if (sourceArtifactId == null && model.getParent() != null) {
            sourceArtifactId = model.getParent().getArtifactId();
        }
        return sourceArtifactId;
    }

    /**
     * Extracts the groupId from a raw model, interpolating from the parent if necessary.
     *
     * @param model The model.
     * @return The groupId.
     */
    public static String getGroupId(Model model) {
        String targetGroupId = model.getGroupId();
        if (targetGroupId == null && model.getParent() != null) {
            targetGroupId = model.getParent().getGroupId();
        }
        return targetGroupId;
    }

    /**
     * Finds the local root of the current project of the {@link MavenSession} instance.
     *
     * @param projectBuilder {@link ProjectBuilder} instance
     * @param mavenSession   {@link MavenSession} instance
     * @param logger         The logger to log tog
     * @return The local root (note this may be the project passed as an argument).
     */
    public static MavenProject getLocalRoot(ProjectBuilder projectBuilder, MavenSession mavenSession, Log logger) {
        logger.info("Searching for local aggregator root...");
        MavenProject project = mavenSession.getCurrentProject();
        while (true) {
            final File parentDir = project.getBasedir().getParentFile();
            if (parentDir != null && parentDir.isDirectory()) {
                logger.debug("Checking to see if " + parentDir + " is an aggregator parent");
                File parentFile = new File(parentDir, "pom.xml");
                if (parentFile.isFile()) {
                    try {
                        ProjectBuildingResult result =
                                projectBuilder.build(parentFile, createProjectBuilderRequest(mavenSession));
                        if (!result.getProblems().isEmpty()) {
                            logger.warn("Problems encountered during the computation of the local aggregation root.");
                            result.getProblems().forEach(p -> logger.warn("\t" + p.getMessage()));
                        }
                        if (getAllChildModules(result.getProject(), logger)
                                .contains(project.getBasedir().getName())) {
                            logger.debug(parentDir + " is an aggregator parent");
                            project = result.getProject();
                            continue;
                        } else {
                            logger.debug(parentDir + " is not an aggregator parent");
                        }
                    } catch (ProjectBuildingException e) {
                        logger.warn(e);
                    }
                }
            }
            logger.debug("Local aggregation root is " + project.getBasedir());
            return project;
        }
    }

    /**
     * <p>Convenience method for creating a {@link ProjectBuildingRequest} instance based on maven session.</p>
     * <p><u>Note:</u> The method initializes the remote repositories with the remote artifact repositories.
     * Please use the initializers if you need to override this.</p>
     *
     * @param mavenSession {@link MavenSession} instance
     * @param initializers optional additional initializers, which will be executed after the object is initialized
     * @return constructed builder request
     */
    @SafeVarargs
    public static ProjectBuildingRequest createProjectBuilderRequest(
            MavenSession mavenSession, Consumer<ProjectBuildingRequest>... initializers) {
        return new DefaultProjectBuildingRequest() {
            {
                setValidationLevel(ModelBuildingRequest.VALIDATION_LEVEL_MINIMAL);
                setResolveDependencies(false);
                setLocalRepository(mavenSession.getLocalRepository());
                setRemoteRepositories(mavenSession.getCurrentProject().getRemoteArtifactRepositories());
                setBuildStartTime(mavenSession.getStartTime());
                setUserProperties(mavenSession.getUserProperties());
                setSystemProperties(mavenSession.getSystemProperties());
                setActiveProfileIds(mavenSession.getRequest().getActiveProfiles());
                setInactiveProfileIds(mavenSession.getRequest().getInactiveProfiles());
                setRepositorySession(mavenSession.getRepositorySession());
                Arrays.stream(initializers).forEach(i -> i.accept(this));
            }
        };
    }

    /**
     * Builds a {@link ModelNode} tree of raw models keyed by module path and returns a list of all nodes,
     * ordered depth-first visiting order. The root node is always the first node of the list.
     *
     * @param rootNode The root node of the reactor
     * @param logger   logger to log parsing errors to
     * @return the root node of the {@link ModelNode} of raw models relative to the project's basedir.
     */
    public static List<ModelNode> getRawModelTree(ModelNode rootNode, Log logger) throws UncheckedIOException {
        Path baseDir = rootNode.getModel().getPomFile().getParentFile().toPath();
        List<ModelNode> result = new ArrayList<>();
        result.add(rootNode);
        result.addAll(getAllChildModules(rootNode.getModel(), logger).stream()
                .map(baseDir::resolve)
                .map(path -> Files.isDirectory(path) ? path.resolve("pom.xml") : path)
                .map(pomFile -> {
                    try {
                        MutableXMLStreamReader pom = new MutableXMLStreamReader(pomFile);
                        return new ModelNode(rootNode, getRawModel(pom.getSource(), pomFile.toFile()), pom);
                    } catch (IOException e) {
                        throw new UncheckedIOException("Could not open " + pomFile, e);
                    } catch (XMLStreamException | TransformerException e) {
                        throw new RuntimeException("Could not parse " + pomFile, e);
                    }
                })
                .flatMap(node -> getRawModelTree(node, logger).stream())
                .collect(Collectors.toList()));
        return result;
    }

    /**
     * Traverses the module tree upwards searching for the closest definition of a property with the given name.
     *
     * @param propertyName name of the property to be found
     * @param node         model tree node at which the search should be started
     * @return {@link Optional} object containing the model tree node containing the closest
     * property definition, or {@link Optional#empty()} if none has been found
     */
    public static Optional<ModelNode> findProperty(String propertyName, ModelNode node) {
        if (ofNullable(node.getModel().getProperties())
                .map(properties -> properties.getProperty(propertyName))
                .isPresent()) {
            return Optional.of(node);
        }
        return node.getParent().flatMap(parent -> findProperty(propertyName, parent));
    }

    /**
     * Builds a map of raw models keyed by module path.
     *
     * @param project The project to build from.
     * @param logger  The logger for logging.
     * @return A map of raw models keyed by path relative to the project's basedir.
     * @throws IOException if things go wrong.
     */
    public static Map<File, Model> getChildModels(MavenProject project, Log logger) throws IOException {
        Map<File, Model> result = new LinkedHashMap<>();
        final Model model = getRawModel(project);
        result.put(project.getFile(), model);
        result.putAll(getChildModels(model, logger));
        return result;
    }

    /**
     * Builds a sub-map of raw models keyed by module path.
     *
     * @param model  The root model
     * @param logger The logger for logging.
     * @return A map of raw models keyed by path relative to the project's basedir.
     */
    private static Map<File, Model> getChildModels(Model model, Log logger) {
        Map<File, Model> result = new LinkedHashMap<>();
        Map<File, Model> childResults = new LinkedHashMap<>();

        File baseDir = model.getPomFile().getParentFile();

        getAllChildModules(model, logger).stream()
                .map(moduleName -> new File(baseDir, moduleName))
                .map(file -> file.isFile() ? file : new File(file, "pom.xml"))
                .filter(File::exists)
                .forEach(pomFile -> {
                    try {
                        // the aim of this goal is to fix problems when the project cannot be parsed by Maven,
                        // so we have to work with the raw model and not the interpolated parsed model from maven
                        Model moduleModel = getRawModel(pomFile);
                        result.put(pomFile, moduleModel);
                        childResults.putAll(getChildModels(moduleModel, logger));
                    } catch (IOException e) {
                        throw new UncheckedIOException(e);
                    }
                });

        result.putAll(childResults); // more efficient update order if all children are added after siblings
        return result;
    }

    /**
     * Returns all the models that have a specified groupId and artifactId as parent.
     *
     * @param reactor    The map of models keyed by path.
     * @param groupId    The groupId of the parent.
     * @param artifactId The artifactId of the parent.
     * @return a map of models that have a specified groupId and artifactId as parent keyed by path.
     */
    public static Map<File, Model> getChildModels(Map<File, Model> reactor, String groupId, String artifactId) {
        final Map<File, Model> result = new LinkedHashMap<>();
        for (Map.Entry<File, Model> entry : reactor.entrySet()) {
            final File path = entry.getKey();
            final Model model = entry.getValue();
            final Parent parent = model.getParent();
            if (parent != null && groupId.equals(parent.getGroupId()) && artifactId.equals(parent.getArtifactId())) {
                result.put(path, model);
            }
        }
        return result;
    }

    /**
     * Returns the model that has the specified groupId and artifactId or <code>null</code> if no such model exists.
     *
     * @param reactor    The map of models keyed by path.
     * @param groupId    The groupId to match.
     * @param artifactId The artifactId to match.
     * @return The model or <code>null</code> if the model was not in the reactor.
     */
    public static Model getModel(Map<File, Model> reactor, String groupId, String artifactId) {
        return reactor.values().stream()
                .filter(model -> (groupId == null || groupId.equals(getGroupId(model)))
                        && artifactId.equals(getArtifactId(model)))
                .findAny()
                .orElse(null);
    }

    /**
     * Returns the model that has the specified groupId (if specified)
     * and artifactId or <code>null</code> if no such model exists.
     *
     * @param reactor    The map of models keyed by path.
     * @param groupId    The groupId to match.
     * @param artifactId The artifactId to match.
     * @return The model entry or <code>null</code> if the model was not in the reactor.
     */
    public static Map.Entry<File, Model> getModelEntry(Map<File, Model> reactor, String groupId, String artifactId) {
        return reactor.entrySet().stream()
                .filter(e -> (groupId == null || groupId.equals(PomHelper.getGroupId(e.getValue())))
                        && artifactId.equals(PomHelper.getArtifactId(e.getValue())))
                .findAny()
                .orElse(null);
    }

    /**
     * Returns a count of how many parents a model has in the reactor.
     *
     * @param reactor The map of models keyed by path.
     * @param model   The model.
     * @return The number of parents of this model in the reactor.
     */
    public static int getReactorParentCount(Map<File, Model> reactor, Model model) {
        if (model.getParent() == null) {
            return 0;
        }
        Model parentModel = getModel(
                reactor, model.getParent().getGroupId(), model.getParent().getArtifactId());
        if (parentModel == null) {
            return 0;
        }
        return getReactorParentCount(reactor, parentModel) + 1;
    }

    /**
     * Reads an XML from the given InputStream.
     *
     * @param inputStream The input stream to read.
     * @return Pair&lt;String, Charset&gt; The (mutable) content of the file with the charset of the file
     * @throws java.io.IOException when things go wrong.
     */
    public static Pair<String, Charset> readXml(InputStream inputStream) throws IOException, XMLStreamException {
        try (BufferedInputStream buffer = new BufferedInputStream(inputStream)) {
            // XMLStreamReader is only used to discover the encoding
            // NB. We can't use a Transformer because XMLStreamReader will override
            // the preamble with its own detected encoding, which is not always what the original file has

            buffer.mark(0x4000);
            XMLInputFactory2 factory = (XMLInputFactory2) XMLInputFactory2.newInstance();
            factory.configureForLowMemUsage();
            XMLStreamReader xmlStreamReader = factory.createXMLStreamReader(buffer);
            xmlStreamReader.close();
            Charset encoding = ofNullable(xmlStreamReader.getEncoding())
                    .map(Charset::forName)
                    .orElse(Charset.defaultCharset());

            buffer.reset();
            return Pair.of(IOUtil.toString(buffer, encoding.toString()), encoding);
        }
    }

    /**
     * Reads an XML from a file.
     *
     * @param file The file to read.
     * @return Pair&lt;String, Charset&gt; The contents of the file with the charset of the file
     * @throws java.io.IOException when things go wrong.
     */
    public static Pair<String, Charset> readXml(File file)
            throws IOException, XMLStreamException, TransformerException {
        try (InputStream is = Files.newInputStream(file.toPath())) {
            return readXml(is);
        }
    }

    /**
     * Returns the GAV coordinates of a model.
     *
     * @param model the model.
     * @return the GAV coordinates of a model.
     */
    public static String getGAV(Model model) {
        return getGroupId(model) + ":" + getArtifactId(model) + ":" + getVersion(model);
    }
}
