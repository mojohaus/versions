package org.codehaus.mojo.versions.change;

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

import java.util.Objects;

import org.codehaus.mojo.versions.api.change.PropertyVersionChange;

/**
 * Represents a change of a property value
 *
 * @deprecated replaced by {@code versions-model}
 */
@Deprecated
public final class DefaultPropertyVersionChange implements PropertyVersionChange {

    private final String property;

    private final String oldValue;

    private final String newValue;

    /**
     * Creates a new instance, providing a property name, old and new values of the property.
     * @param property property name
     * @param oldValue old property value
     * @param newValue new property value
     */
    public DefaultPropertyVersionChange(String property, String oldValue, String newValue) {
        this.property = property;
        this.oldValue = oldValue;
        this.newValue = newValue;
    }

    /**
     * Returns the property name
     * @return property name
     */
    public String getProperty() {
        return property;
    }

    /**
     * Returns the old value of the property
     * @return old property value
     */
    public String getOldValue() {
        return oldValue;
    }

    /**
     * Returns the new value of the property
     * @return new property value
     */
    public String getNewValue() {
        return newValue;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        DefaultPropertyVersionChange versionChange = (DefaultPropertyVersionChange) o;

        if (!Objects.equals(property, versionChange.property)) {
            return false;
        }
        if (!Objects.equals(oldValue, versionChange.oldValue)) {
            return false;
        }
        return Objects.equals(newValue, versionChange.newValue);
    }

    @Override
    public int hashCode() {
        int result = property != null ? property.hashCode() : 0;
        result = 7 * result + (oldValue != null ? oldValue.hashCode() : 0);
        result = 31 * result + (newValue != null ? newValue.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "DefaultPropertyVersionChange(" + property + ':' + oldValue + "-->" + newValue + ')';
    }
}
