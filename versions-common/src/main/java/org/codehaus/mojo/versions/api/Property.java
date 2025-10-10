package org.codehaus.mojo.versions.api;

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

import org.apache.maven.model.Dependency;

/**
 * Represents an association between properties and dependencies.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class Property {
    /**
     * The property that defines the version of the artifact to use.
     *
     * @parameter
     * @required
     * @since 1.0-alpha-3
     */
    private String name;

    /**
     * The (optional) version range that the property must always be within.
     *
     * @parameter
     * @since 1.0-alpha-3
     */
    private String version;

    /**
     * Used by {@link org.codehaus.mojo.versions.utils.PropertyComparator} when comparing two same named properties
     *
     * @since 2.14.0
     */
    private String value;

    /**
     * Whether to automatically link dependencies to the property.
     *
     * @parameter default-value="true"
     * @since 1.0-alpha-3
     */
    private boolean autoLinkDependencies;

    /**
     * Dependencies that must be available for this property. If {@link #autoLinkDependencies} is true then these
     * dependencies will be considered in addition to the automatically linked dependencies.
     *
     * @parameter
     * @since 1.0-alpha-3
     */
    private Dependency[] dependencies;

    /**
     * Whether the reactor can be used as a source of artifact versions.
     *
     * @parameter default-value="true"
     * @since 1.0-alpha-3
     */
    private boolean searchReactor;

    /**
     * When {@link #searchReactor} is {@code true} and a property version can be entirely satisfied from the
     * reactor and this setting is {@code true} then the reactor version will be specified irrespective of any
     * other settings (including {@link #isBanSnapshots}).
     *
     * @since 1.0-alpha-3
     */
    private boolean preferReactor;

    /**
     * Whether the snapshot versions cannot be used as an artifact versions (unless {@link #preferReactor} applies).
     *
     * @parameter default-value="false"
     * @since 1.0-alpha-3
     */
    private boolean banSnapshots;

    private static final Dependency[] EMPTY_DEPENDENCY_ARRAY = new Dependency[0];

    /**
     * Creates a new instance without any set name
     */
    public Property() {
        this.autoLinkDependencies = true;
    }

    /**
     * Creates a new {@link Property} with the given name
     * @param name name of the property
     */
    public Property(String name) {
        this.name = name;
        this.autoLinkDependencies = true;
        this.dependencies = EMPTY_DEPENDENCY_ARRAY;
        this.banSnapshots = false;
        this.searchReactor = true;
        this.preferReactor = true;
        this.version = null;
        this.value = null;
    }

    /**
     * Returns the property name
     *
     * @return name of the property
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the property
     *
     * @param name name to be set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Returns the property version
     *
     * @return version
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the property version
     *
     * @param version version to be set
     */
    public void setVersion(String version) {
        this.version = version;
    }

    /**
     * Whether properties linking versions should be auto-detected or not
     *
     * @return {@code true} if properties linking versions should be auto-detected or not
     */
    public boolean isAutoLinkDependencies() {
        return autoLinkDependencies;
    }

    /**
     * Specifies whether properties indicating dependency versions should be linked automatically
     *
     * @param autoLinkDependencies {@code true} if properties indicating dependency versions should
     *                             be linked automatically
     */
    public void setAutoLinkDependencies(boolean autoLinkDependencies) {
        this.autoLinkDependencies = autoLinkDependencies;
    }

    /**
     * Dependencies that must be available for this property. If {@link #isAutoLinkDependencies} is {@code true} then
     * these dependencies will be considered in addition to the automatically linked dependencies.
     *
     * @return dependencies
     */
    public Dependency[] getDependencies() {
        return dependencies;
    }

    /**
     * Dependencies that must be available for this property. If {@link #isAutoLinkDependencies} is {@code true} then
     * these dependencies will be considered in addition to the automatically linked dependencies.
     *
     * @param dependencies to be set
     */
    public void setDependencies(Dependency[] dependencies) {
        this.dependencies = dependencies;
    }

    /**
     * Says whether reactor should be searched for the artifact version
     *
     * @return {@code true} if reactor should be searched for the artifact version
     */
    public boolean isSearchReactor() {
        return searchReactor;
    }

    /**
     * Specifies whether reactor should be searched for the artifact version
     *
     * @param searchReactor whether reactor should be searched for the artifact version
     */
    public void setSearchReactor(boolean searchReactor) {
        this.searchReactor = searchReactor;
    }

    /**
     * Says whether, when {@link #searchReactor} is {@code true} and a property version can be entirely satisfied
     * from the reactor and this setting is {@code true} then the reactor version will be specified irrespective of any
     * other settings (including {@link #isBanSnapshots}).
     * @return provided value
     */
    public boolean isPreferReactor() {
        return preferReactor;
    }

    /**
     * Specifies whether, when {@link #searchReactor} is {@code true} and a property version can be entirely satisfied
     * from the reactor and this setting is {@code true} then the reactor version will be specified irrespective of any
     * other settings (including {@link #isBanSnapshots}).
     *
     * @param preferReactor required value
     */
    public void setPreferReactor(boolean preferReactor) {
        this.preferReactor = preferReactor;
    }

    /**
     * Specifies whether snapshots should not be considered
     * @return {@code true} if snapshots should not be considered
     */
    public boolean isBanSnapshots() {
        return banSnapshots;
    }

    /**
     * Specifies whether snapshots should not be considered
     * @param banSnapshots {@code true} if snapshots should not be considered
     */
    public void setBanSnapshots(boolean banSnapshots) {
        this.banSnapshots = banSnapshots;
    }

    /**
     * Returns the value of the property
     * @return value of the property
     */
    public String getValue() {
        return value;
    }

    /**
     * Sets the value of the property
     * @param value property value to be set
     */
    public void setValue(String value) {
        this.value = value;
    }
}
