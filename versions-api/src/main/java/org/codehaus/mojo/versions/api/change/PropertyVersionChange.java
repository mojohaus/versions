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
 * Represents a change of a property value.
 *
 * @author Andrzej Jarmoniuk
 * @since 2.15.0
 * @deprecated use classes from {@code versions-model}
 */
@Deprecated
public interface PropertyVersionChange extends VersionChange {

    /**
     * Returns the name of the property
     * @return name of the property
     */
    String getProperty();

    /**
     * Returns the old value of the property
     * @return old value of the property
     */
    String getOldValue();

    /**
     * Returns the new value of the property
     * @return new value of the property
     */
    String getNewValue();
}
