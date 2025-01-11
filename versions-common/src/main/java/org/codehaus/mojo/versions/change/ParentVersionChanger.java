package org.codehaus.mojo.versions.change;

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

import javax.xml.stream.XMLStreamException;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.change.DependencyVersionChange;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;

/**
 * Version changer for the parent
 */
public class ParentVersionChanger extends AbstractVersionChanger {

    /**
     * Constructs a new instance
     * @param model {@link Model} instance
     * @param pom {@link MutableXMLStreamReader} representing the pom file to be modified
     * @param reporter {@link Log} object
     */
    public ParentVersionChanger(Model model, MutableXMLStreamReader pom, Log reporter) {
        super(model, pom, reporter);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void apply(DependencyVersionChange versionChange) throws XMLStreamException {
        if (getModel().getParent() != null
                && versionChange.getGroupId().equals(getModel().getParent().getGroupId())
                && versionChange.getArtifactId().equals(getModel().getParent().getArtifactId())) {
            if (PomHelper.setProjectParentVersion(getPom(), versionChange.getNewVersion())) {
                getLog().info("    Updating parent " + versionChange.getGroupId() + ":"
                        + versionChange.getArtifactId());
                getLog().info("        from version " + versionChange.getOldVersion() + " to "
                        + versionChange.getNewVersion());
            }
        }
    }
}
