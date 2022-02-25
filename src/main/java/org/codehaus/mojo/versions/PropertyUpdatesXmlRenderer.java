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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.shared.utils.xml.PrettyPrintXMLWriter;
import org.apache.maven.shared.utils.xml.XMLWriter;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.utils.PropertyComparator;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

/**
 * @author Aleksei Grigorov
 * @since 2.9.1
 */
public class PropertyUpdatesXmlRenderer {

  private final Map<Property, PropertyVersions> propertyUpdates;
  private final String outputFileName;

  public PropertyUpdatesXmlRenderer(Map<Property, PropertyVersions> propertyUpdates, String outputFileName) {
    TreeMap<Property, PropertyVersions> allUpdates = new TreeMap<>(new PropertyComparator());
    allUpdates.putAll(propertyUpdates);

    this.propertyUpdates = Collections.unmodifiableSortedMap(allUpdates);
    this.outputFileName = outputFileName;
  }

  /**
   * Renders report to xml file in target directory.
   *
   * @throws MavenReportException if something went wrong
   */
  public void render() throws MavenReportException {
    try (PrintWriter fileWriter = new PrintWriter(outputFileName, "UTF8")) {
      XMLWriter xml = new PrettyPrintXMLWriter(fileWriter);
      xml.setEncoding("UTF-8");
      xml.startElement("PropertyUpdatesReport");
      renderSummary(xml, propertyUpdates);
      xml.startElement("properties");
      for (Map.Entry<Property, PropertyVersions> entry : propertyUpdates.entrySet()) {
        renderPropertyDetails(xml, entry.getKey(), entry.getValue());
      }
      xml.endElement();
      xml.endElement();
    } catch (IOException e) {
      throw new MavenReportException("Cannot create xml report", e);
    }
  }

  @SuppressWarnings("Duplicates")
  private static void renderSummary(XMLWriter xml, Map<Property, PropertyVersions> allUpdates) throws IOException {
    int numInc = 0;
    int numMin = 0;
    int numMaj = 0;
    int numAny = 0;
    int numCur = 0;

    for (PropertyVersions details : allUpdates.values()) {
      if (details.getOldestUpdate(UpdateScope.SUBINCREMENTAL) != null) {
        numAny++;
      } else if (details.getOldestUpdate(UpdateScope.INCREMENTAL) != null) {
        numInc++;
      } else if (details.getOldestUpdate(UpdateScope.MINOR) != null) {
        numMin++;
      } else if (details.getOldestUpdate(UpdateScope.MAJOR) != null) {
        numMaj++;
      } else {
        numCur++;
      }
    }

    xml.startElement("summary");

    xml.startElement("usingLastVersion");
    xml.writeText(Integer.toString(numCur));
    xml.endElement();

    xml.startElement("nextVersionAvailable");
    xml.writeText(Integer.toString(numAny));
    xml.endElement();

    xml.startElement("nextIncrementalAvailable");
    xml.writeText(Integer.toString(numInc));
    xml.endElement();

    xml.startElement("nextMinorAvailable");
    xml.writeText(Integer.toString(numMin));
    xml.endElement();

    xml.startElement("nextMajorAvailable");
    xml.writeText(Integer.toString(numMaj));
    xml.endElement();

    xml.endElement();
  }

  private static void renderPropertyDetails(XMLWriter xml, Property property, PropertyVersions versions) throws IOException {
    xml.startElement("property");

    xml.startElement("name");
    xml.writeText(property.getName());
    xml.endElement();

    if (versions.getAssociations().length != 0) {
      xml.startElement("associations");
      for (ArtifactAssociation association : versions.getAssociations()) {
        xml.startElement("association");

        xml.startElement("groupId");
        xml.writeText(association.getGroupId());
        xml.endElement();

        xml.startElement("artifactId");
        xml.writeText(association.getArtifactId());
        xml.endElement();

        xml.endElement();
      }
      xml.endElement();
    }

    xml.startElement("currentVersion");
    xml.writeText(versions.getCurrentVersion().toString());
    xml.endElement();

    String status = "no new available";
    ArtifactVersion nextVersion = versions.getOldestUpdate(UpdateScope.ANY);
    if (nextVersion != null) {
      for (UpdateScope scope : UpdateScope.values()) {
        if (scope == UpdateScope.ANY) {
          continue;
        }
        boolean hasUpdates = renderScopeDetails(xml, versions, scope);
        if (hasUpdates) {
          status = scope.name().toLowerCase() + " available";
        }
      }
    }

    xml.startElement("status");
    xml.writeText(status);
    xml.endElement();

    xml.endElement();
  }

  private static boolean renderScopeDetails(XMLWriter xml, PropertyVersions versions, UpdateScope scope) throws IOException {
    String tag = scope.name().toLowerCase();
    ArtifactVersion[] updates = versions.getAllUpdates(scope);
    if (updates == null || updates.length == 0) {
      return false;
    } else {
      xml.startElement(tag + "s");
      for (ArtifactVersion update : updates) {
        xml.startElement(tag);
        xml.writeText(update.toString());
        xml.endElement();
      }
      xml.endElement();
      return true;
    }
  }

}
