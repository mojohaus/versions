package org.codehaus.mojo.versions.api.recording;

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
 *
 */

import org.codehaus.mojo.versions.api.change.VersionChange;

/**
 * Represents a change record of an item's version.
 *
 * @author Slawomir Jaranowski
 * @since 2.14.0
 */
public interface ChangeRecord
{
    /**
     * Describe where version item is updated.
     */
    enum ChangeKind
    {
        DEPENDENCY( "dependency-update" ),
        DEPENDENCY_MANAGEMENT( "dependency-management-update" ),
        PARENT( "parent-update" ),
        PLUGIN( "plugin-update" ),
        PLUGIN_MANAGEMENT( "plugin-management-update" ),
        PROPERTY( "property-update" );

        private final String label;

        ChangeKind( String label )
        {
            this.label = label;
        }

        public String getLabel()
        {
            return label;
        }
    }

    /**
     * @return a version item change kind
     * @since 2.14.0
     */
    ChangeKind getKind();

    /**
     * @return a details about changed item
     * @since 2.14.0
     */
    VersionChange getVersionChange();
}
