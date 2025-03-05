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
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name name to be set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return version
     */
    public String getVersion() {
        return version;
    }

    /**
     * @param version version to be set
     */
    public void setVersion(String version) {
        this.version = version;
    }

    /**
     * @return if properties linking versions should be auto-detected or not
     */
    public boolean isAutoLinkDependencies() {
        return autoLinkDependencies;
    }

    /**
     * @param autoLinkDependencies whether properties indicating dependency versions should
     *                             be linked automatically
     */
    public void setAutoLinkDependencies(boolean autoLinkDependencies) {
        this.autoLinkDependencies = autoLinkDependencies;
    }

    /**
     * Dependencies that must be available for this property. If {@link #isAutoLinkDependencies} is {@code true} then
     * these dependencies will be considered in addition to the automatically linked dependencies.
     * @return dependencies
     */
    public Dependency[] getDependencies() {
        return dependencies;
    }

    /**
     * Dependencies that must be available for this property. If {@link #isAutoLinkDependencies} is {@code true} then
     * these dependencies will be considered in addition to the automatically linked dependencies.
     * @param dependencies to be set
     */
    public void setDependencies(Dependency[] dependencies) {
        this.dependencies = dependencies;
    }

    /**
     * @return whether reactor should be searched for the artifact version
     */
    public boolean isSearchReactor() {
        return searchReactor;
    }

    /**
     * @param searchReactor whether reactor should be searched for the artifact version
     */
    public void setSearchReactor(boolean searchReactor) {
        this.searchReactor = searchReactor;
    }

    /**
     * @return when {@link #searchReactor} is {@code true} and a property version can be entirely satisfied from the
     * reactor and this setting is {@code true} then the reactor version will be specified irrespective of any
     * other settings (including {@link #isBanSnapshots}).
     */
    public boolean isPreferReactor() {
        return preferReactor;
    }

    public void setPreferReactor(boolean preferReactor) {
        this.preferReactor = preferReactor;
    }

    public boolean isBanSnapshots() {
        return banSnapshots;
    }

    public void setBanSnapshots(boolean banSnapshots) {
        this.banSnapshots = banSnapshots;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
