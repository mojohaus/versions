package org.codehaus.mojo.versions.xml;

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

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactVersionsCache;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.reporting.PluginOverviewStats;
import org.codehaus.mojo.versions.reporting.model.PluginInfo;
import org.codehaus.mojo.versions.reporting.model.PluginReportSummary;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesReport;
import org.codehaus.mojo.versions.reporting.model.io.xpp3.PluginUpdatesReportXpp3Writer;
import org.codehaus.mojo.versions.reporting.util.ReportRenderer;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.xml.CommonXmlReportRendererUtils.setSection;
import static org.codehaus.mojo.versions.xml.CommonXmlReportRendererUtils.statusFor;

/**
 * XML renderer for DependencyUpdatesReport creates an xml file in target directory and writes report about available
 * dependency/dependency management updates.
 *
 * @author Illia Dubinin
 * @since 2.4
 */
public class PluginUpdatesXmlReportRenderer implements ReportRenderer {
    private final PluginUpdatesModel model;
    private final Path outputFile;
    private final ArtifactVersionsCache newestUpdateCache =
            new ArtifactVersionsCache(AbstractVersionDetails::getNewestUpdateWithinSegment);

    private final boolean allowSnapshots;

    /**
     * Creates a new instance
     * @param model object containing the updates model
     * @param outputFile output file for the report
     */
    public PluginUpdatesXmlReportRenderer(PluginUpdatesModel model, Path outputFile, boolean allowSnapshots) {
        this.model = model;
        this.outputFile = outputFile;
        this.allowSnapshots = allowSnapshots;
    }

    @Override
    public String getTitle() {
        return "Plugin updates";
    }

    @Override
    public boolean isAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * Creates an XML report
     */
    @Override
    public void render() {
        try (BufferedWriter writer = Files.newBufferedWriter(outputFile, StandardCharsets.UTF_8)) {
            new PluginUpdatesReportXpp3Writer().write(writer, new PluginUpdatesReport() {
                {
                    setSummary(new PluginReportSummary() {
                        {
                            PluginOverviewStats overviewStats = PluginOverviewStats.fromUpdates(
                                    model.getAllUpdates().values(), newestUpdateCache, isAllowSnapshots());
                            setUsingLastVersion(String.valueOf(overviewStats.getUpToDate()));
                            setNextVersionAvailable(String.valueOf(overviewStats.getAny()));
                            setNextIncrementalAvailable(String.valueOf(overviewStats.getIncremental()));
                            setNextMinorAvailable(String.valueOf(overviewStats.getMinor()));
                            setNextMajorAvailable(String.valueOf(overviewStats.getMajor()));
                            setDependencyUpdates(String.valueOf(overviewStats.getDependencies()));
                        }
                    });
                    setPluginManagements(createPluginInfo(model.getArtifactManagementUpdates(), isAllowSnapshots()));
                    setPlugins(createPluginInfo(model.getArtifactUpdates(), isAllowSnapshots()));
                }
            });
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static List<PluginInfo> createPluginInfo(
            Map<Dependency, PluginUpdatesDetails> versions, boolean allowSnapshots) {
        return versions.entrySet().stream()
                .map(e -> new PluginInfo() {
                    {
                        setGroupId(e.getKey().getGroupId());
                        setArtifactId(e.getKey().getArtifactId());
                        setCurrentVersion(e.getKey().getVersion());
                        setScope(e.getKey().getScope());
                        setType(e.getKey().getType());
                        setClassifier(e.getKey().getClassifier());

                        ofNullable(e.getValue().getNewestUpdateWithinSegment(empty(), allowSnapshots))
                                .map(ArtifactVersion::toString)
                                .ifPresent(this::setLastVersion);

                        setSection(e.getValue(), INCREMENTAL, this::setIncrementals, allowSnapshots);
                        setSection(e.getValue(), MINOR, this::setMinors, allowSnapshots);
                        setSection(e.getValue(), MAJOR, this::setMajors, allowSnapshots);

                        setStatus(statusFor(getLastVersion(), getIncrementals(), getMinors()));
                    }
                })
                .collect(Collectors.toList());
    }
}
