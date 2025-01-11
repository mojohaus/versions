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

import java.util.Locale;
import java.util.Map;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.ParentUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates a report on available updates for parent artifacts
 *
 * @author Andrzej Jarmoniuk
 * @since 2.13.0
 */
@Mojo(name = "parent-updates-report", requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true)
public class ParentUpdatesReport extends AbstractVersionsReport<ParentUpdatesModel> {

    @Inject
    protected ParentUpdatesReport(
            I18N i18n,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactFactory, repositorySystem, wagonMap, rendererFactory);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isExternalReport() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean canGenerateReport() {
        if (getProject().getParent() == null) {
            getLog().warn("Project does not have a parent.");
            return false;
        }

        if (reactorProjects.contains(getProject().getParent())) {
            getLog().warn("Parent project is part of the reactor.");
            return false;
        }

        return true;
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    protected void doGenerateReport(Locale locale, Sink sink) throws MavenReportException {
        try {
            ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions(project.getParentArtifact(), false);
            rendererFactory
                    .createReportRenderer(
                            getBundleName(),
                            sink,
                            locale,
                            new ParentUpdatesModel(
                                    DependencyBuilder.newBuilder()
                                            .withGroupId(artifactVersions.getGroupId())
                                            .withArtifactId(artifactVersions.getArtifactId())
                                            .withVersion(artifactVersions
                                                    .getArtifact()
                                                    .getVersion())
                                            .withScope(artifactVersions
                                                    .getArtifact()
                                                    .getScope())
                                            .withType(artifactVersions
                                                    .getArtifact()
                                                    .getType())
                                            .withClassifier(artifactVersions
                                                    .getArtifact()
                                                    .getClassifier())
                                            .build(),
                                    artifactVersions),
                            allowSnapshots)
                    .render();
        } catch (VersionRetrievalException e) {
            throw new MavenReportException(e.getMessage(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBundleName() {
        return "parent-updates-report";
    }
}
