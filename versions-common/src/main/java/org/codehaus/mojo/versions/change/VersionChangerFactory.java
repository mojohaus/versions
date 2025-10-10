package org.codehaus.mojo.versions.change;

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

import java.util.ArrayList;
import java.util.List;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;

/**
 * Factory for creating {@link VersionChanger} instances.
 *
 * <p>This factory must be initialized with a Maven {@link Model}, a mutable POM reader
 * {@link MutableXMLStreamReader} and a {@link Log} before creating changers.</p>
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 16:05:27
 */
public class VersionChangerFactory {

    /**
     * Public no-arg constructor to provide an explicit constructor comment for Javadoc.
     */
    public VersionChangerFactory() {
        // no-op constructor
    }

    private Model model = null;

    private MutableXMLStreamReader pom = null;

    private Log log = null;

    /**
     * Returns the Maven model currently set on this factory.
     *
     * @return the configured {@link Model}, never {@code null} after proper initialization
     */
    public synchronized Model getModel() {
        return model;
    }

    /**
     * Sets the Maven model to be used by changers produced by this factory.
     *
     * @param model the {@link Model} to set; must not be {@code null}
     */
    public synchronized void setModel(Model model) {
        this.model = model;
    }

    /**
     * Returns the mutable POM XML reader currently set on this factory.
     *
     * @return the configured {@link MutableXMLStreamReader}
     */
    public synchronized MutableXMLStreamReader getPom() {
        return pom;
    }

    /**
     * Sets the mutable POM XML reader to be used by changers produced by this factory.
     *
     * @param pom the {@link MutableXMLStreamReader} to set; must not be {@code null}
     */
    public synchronized void setPom(MutableXMLStreamReader pom) {
        this.pom = pom;
    }

    /**
     * Returns the logger currently set on this factory.
     *
     * @return the configured {@link Log}
     */
    public synchronized Log getLog() {
        return log;
    }

    /**
     * Sets the logger to be used by changers produced by this factory.
     *
     * @param log the {@link Log} to set; must not be {@code null}
     */
    public synchronized void setLog(Log log) {
        this.log = log;
    }

    private synchronized void checkState() {
        if (model == null) {
            throw new IllegalStateException("Model has not been specified");
        }
        if (pom == null) {
            throw new IllegalStateException("Pom has not been specified");
        }
        if (log == null) {
            throw new IllegalStateException("Log has not been specified");
        }
    }

    /**
     * Creates a {@link PluginVersionChanger} configured with the factory's state.
     *
     * @return a new {@link PluginVersionChanger}
     * @throws IllegalStateException if the factory has not been fully initialized
     */
    public synchronized VersionChanger newPluginVersionChanger() {
        checkState();
        return new PluginVersionChanger(model, pom, log);
    }

    /**
     * Creates a {@link DependencyVersionChanger} configured with the factory's state.
     *
     * @return a new {@link DependencyVersionChanger}
     * @throws IllegalStateException if the factory has not been fully initialized
     */
    public synchronized VersionChanger newDependencyVersionChanger() {
        checkState();
        return new DependencyVersionChanger(model, pom, log);
    }

    /**
     * Creates a {@link ProjectVersionChanger} configured with the factory's state.
     *
     * @return a new {@link ProjectVersionChanger}
     * @throws IllegalStateException if the factory has not been fully initialized
     */
    public synchronized VersionChanger newProjectVersionChanger() {
        checkState();
        return new ProjectVersionChanger(model, pom, log);
    }

    /**
     * Creates a {@link ParentVersionChanger} configured with the factory's state.
     *
     * @return a new {@link ParentVersionChanger}
     * @throws IllegalStateException if the factory has not been fully initialized
     */
    public synchronized VersionChanger newParentVersionChanger() {
        checkState();
        return new ParentVersionChanger(model, pom, log);
    }

    /**
     * Creates a composite {@link VersionChanger} that will process parent, project, dependencies and plugins
     * (in that order) using the factory's state.
     *
     * @return a new {@link CompositeVersionChanger}
     * @throws IllegalStateException if the factory has not been fully initialized
     */
    public synchronized VersionChanger newVersionChanger() {
        checkState();
        VersionChanger[] delegates = new VersionChanger[] {
            newParentVersionChanger(),
            newProjectVersionChanger(),
            newDependencyVersionChanger(),
            newPluginVersionChanger()
        };
        return new CompositeVersionChanger(delegates);
    }

    /**
     * Creates a composite {@link VersionChanger} that includes only the requested processors.
     *
     * @param processParent       if true include a {@link ParentVersionChanger}
     * @param processProject      if true include a {@link ProjectVersionChanger}
     * @param processDependencies if true include a {@link DependencyVersionChanger}
     * @param processPlugins      if true include a {@link PluginVersionChanger}
     * @return a new {@link CompositeVersionChanger} composed of the selected changers
     * @throws IllegalStateException if the factory has not been fully initialized
     */
    public synchronized VersionChanger newVersionChanger(
            boolean processParent, boolean processProject, boolean processDependencies, boolean processPlugins) {
        checkState();

        List<VersionChanger> delegates = new ArrayList<>();

        if (processParent) {
            delegates.add(newParentVersionChanger());
        }
        if (processProject) {
            delegates.add(newProjectVersionChanger());
        }
        if (processDependencies) {
            delegates.add(newDependencyVersionChanger());
        }
        if (processPlugins) {
            delegates.add(newPluginVersionChanger());
        }

        return new CompositeVersionChanger(delegates);
    }
}
