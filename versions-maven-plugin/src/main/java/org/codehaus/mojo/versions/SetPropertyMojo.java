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
import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.PropertiesVersionsFileReader;
import org.eclipse.aether.RepositorySystem;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.split;

/**
 * Set a property to a given version without any sanity checks. Please be careful this can lead to changes which might
 * not build anymore. The sanity checks are done by other goals like <code>update-properties</code> or
 * <code>update-property</code> etc. they are not done here. So use this goal with care.
 *
 * @author Karl Heinz Marbaise
 * @since 2.5
 */
@Mojo(name = "set-property", threadSafe = true)
public class SetPropertyMojo extends AbstractVersionsUpdaterMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * A property to update.
     * You can also specify multiple property names separated by "," which are all set to the same new version.
     */
    @Parameter(property = "property")
    private String property = null;

    /**
     * The new version to set the property.
     */
    @Parameter(property = "newVersion")
    private String newVersion = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     */
    @Parameter(property = "autoLinkItems", defaultValue = "true")
    private boolean autoLinkItems;

    /**
     * A property file name containing: property=value, to update several properties at the same time.
     * If 'property' and 'newVersion' are also used, they will be ignored.
     *
     * @since 2.9
     */
    @Parameter(property = "propertiesVersionsFile")
    private String propertiesVersionsFile;

    /**
     * The Maven profile to apply the changes. If the provided profile is not found, no changes will be applied
     *
     * @since 2.15
     */
    @Parameter(property = "profileId")
    private String profileId = null;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    // --------------------------- CONSTRUCTORS ---------------------------

    /**
     * Creates a new instance.
     *
     * @param artifactFactory the artifact factory
     * @param repositorySystem the repository system
     * @param wagonMap the map of wagon implementations
     * @param changeRecorderFactories the change recorder factories
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public SetPropertyMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        List<Property> propertiesConfig;
        String properties;
        if (!isEmpty(propertiesVersionsFile)) {
            logWrongConfigWarning();
            getLog().debug("Reading properties and versions to update from file: " + propertiesVersionsFile);
            PropertiesVersionsFileReader reader = new PropertiesVersionsFileReader(propertiesVersionsFile);
            try {
                reader.read();
            } catch (IOException e) {
                getLog().error("Unable to read property file  " + propertiesVersionsFile
                        + ". re-run with -X option for more details.");
                getLog().debug("Error while reading  property file " + propertiesVersionsFile, e);
                throw new MojoFailureException("Unable to read property file " + propertiesVersionsFile);
            }
            propertiesConfig = reader.getPropertiesConfig();
            properties = reader.getProperties();
        } else if (!isEmpty(property)) {
            getLog().debug("Reading properties and versions to update from property and newVersion ");
            propertiesConfig = Arrays.stream(split(property, ","))
                    .map(prp -> {
                        Property propertyConfig = new Property(prp);
                        propertyConfig.setVersion(newVersion);
                        return propertyConfig;
                    })
                    .collect(Collectors.toList());
            properties = property;
        } else {
            throw new MojoExecutionException("Please provide either 'property' or 'propertiesVersionsFile' parameter.");
        }
        update(pom, propertiesConfig, properties);
    }

    private void update(MutableXMLStreamReader pom, List<Property> propertiesConfig, String properties)
            throws MojoExecutionException, XMLStreamException {
        Map<Property, PropertyVersions> propertyVersions = this.getHelper()
                .getVersionPropertiesMap(VersionsHelper.VersionPropertiesMapRequest.builder()
                        .withMavenProject(getProject())
                        .withPropertyDefinitions(propertiesConfig)
                        .withIncludeProperties(properties)
                        .withAutoLinkItems(autoLinkItems)
                        .build());
        for (Map.Entry<Property, PropertyVersions> entry : propertyVersions.entrySet()) {
            Property currentProperty = entry.getKey();
            PropertyVersions version = entry.getValue();
            String newVersionGiven = currentProperty.getVersion();
            String profileToApply;
            String currentVersion;
            if (isEmpty(profileId)) {
                profileToApply = version.getProfileId();
                currentVersion = getProject().getProperties().getProperty(currentProperty.getName());
            } else {
                profileToApply = profileId;
                currentVersion = getProject().getModel().getProfiles().stream()
                        .filter(profile -> profileId.equals(profile.getId()))
                        .findFirst()
                        .map(profile -> profile.getProperties().getProperty(currentProperty.getName()))
                        .orElse(null);
            }
            if (currentVersion == null) {
                continue;
            }
            PomHelper.setPropertyVersion(
                    pom, profileToApply, currentProperty.getName(), defaultString(newVersionGiven));
        }
    }

    private void logWrongConfigWarning() {
        if (!isEmpty(property)) {
            getLog().warn("-Dproperty provided but will be ignored as -DpropertiesVersionsFile is used");
        }
        if (!isEmpty(newVersion)) {
            getLog().warn("-DnewVersion provided but will be ignored as -DpropertiesVersionsFile is used");
        }
    }
}
