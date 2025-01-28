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

import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 15:59:46
 */
public abstract class AbstractVersionChanger implements VersionChanger {
    /**
     * {@link Model} instance
     */
    private final Model model;

    /**
     * {@link MutableXMLStreamReader} instance
     */
    private final MutableXMLStreamReader pom;

    /**
     * {@link Log} instance
     */
    private final Log log;

    /**
     * Creates a new instance providing the model, mutable pom, and a logger.
     * @param model {@link Model} instance
     * @param pom {@link MutableXMLStreamReader} instance representing the parsed (mutable) pom file
     * @param log {@link Log} instance for logging
     */
    public AbstractVersionChanger(Model model, MutableXMLStreamReader pom, Log log) {
        this.model = model;
        this.pom = pom;
        this.log = log;
    }

    /**
     * Returns the Maven Model instance
     * @return {@link Model} instance
     */
    protected Model getModel() {
        return model;
    }

    /**
     * Returns the {@link MutableXMLStreamReader} instance representing the parsed and possibly modified POM file.
     * @return {@link MutableXMLStreamReader} instance under changes
     */
    protected MutableXMLStreamReader getPom() {
        return pom;
    }

    /**
     * Returns the {@link Log} instance
     * @return {@link Log} instance
     */
    protected Log getLog() {
        return log;
    }
}
