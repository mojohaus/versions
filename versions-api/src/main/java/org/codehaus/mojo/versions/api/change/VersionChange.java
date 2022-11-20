package org.codehaus.mojo.versions.api.change;

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

/**
 * Represents a change of an item's version.
 *
 * @author Slawomir Jaranowski
 * @since 2.14.0
 */
public interface VersionChange
{
    /**
     * @return a groupId of changed item
     * @since 2.14.0
     */
    String getGroupId();

    /**
     * @return an ArtifactId of change item
     * @since 2.14.0
     */
    String getArtifactId();

    /**
     * @return an old version of changed item
     * @since 2.14.0
     */
    String getOldVersion();

    /**
     * @return a new version of changed item
     * @since 2.14.0
     */
    String getNewVersion();

}
