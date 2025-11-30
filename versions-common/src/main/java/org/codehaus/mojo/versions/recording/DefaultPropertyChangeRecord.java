package org.codehaus.mojo.versions.recording;

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
import org.codehaus.mojo.versions.api.recording.PropertyChangeRecord;
import org.codehaus.mojo.versions.change.DefaultPropertyVersionChange;

/**
 * Represents a change record of a property version.
 *
 * @author Slawomir Jaranowski
 * @since 2.14.0
 * @deprecated replaced by {@link org.codehaus.mojo.versions.model.PropertyVersionChange}
 */
@Deprecated
public class DefaultPropertyChangeRecord implements PropertyChangeRecord {
    private final PropertyVersionChange versionChange;

    private DefaultPropertyChangeRecord(PropertyVersionChange versionChange) {
        this.versionChange = Objects.requireNonNull(versionChange, "versionChange must not be null");
    }

    @Override
    public PropertyVersionChange getVersionChange() {
        return versionChange;
    }

    /**
     * Returns a new {@link DefaultPropertyChangeRecord.Builder} instance.
     * @return a new {@link DefaultPropertyChangeRecord.Builder} instance
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Creates a new {@link Builder} instance
     */
    public static class Builder {
        private String property;
        private String oldValue;
        private String newValue;

        private Builder() {}

        /**
         * Provides a property name
         * @param property property name
         * @return {@link Builder} instance
         */
        public Builder withProperty(String property) {
            this.property = property;
            return this;
        }

        /**
         * Provides the old value of the property
         * @param oldValue old property value
         * @return {@link Builder} instance
         */
        public Builder withOldValue(String oldValue) {
            this.oldValue = oldValue;
            return this;
        }

        /**
         * Provides the new value of the property
         * @param newValue new property value
         * @return {@link Builder} instance
         */
        public Builder withNewValue(String newValue) {
            this.newValue = newValue;
            return this;
        }

        /**
         * Builds a new {@link PropertyChangeRecord} instance, based on the values provided to the builder.
         * @return new {@link PropertyChangeRecord} instance
         */
        public PropertyChangeRecord build() {
            return new DefaultPropertyChangeRecord(new DefaultPropertyVersionChange(property, oldValue, newValue));
        }
    }
}
