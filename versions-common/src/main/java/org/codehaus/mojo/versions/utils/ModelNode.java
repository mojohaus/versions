package org.codehaus.mojo.versions.utils;
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

import java.util.Optional;

import org.apache.maven.model.Model;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Represents a navigable tree of {@link Model} instances.
 */
public class ModelNode {
    private ModelNode parent;
    private Model item;
    private ModifiedPomXMLEventReader pom;

    /**
     * Creates a root node (without a parent).
     *
     * @param model {@link Model} instance
     * @param pom {@link ModifiedPomXMLEventReader} instance
     */
    public ModelNode(Model model, ModifiedPomXMLEventReader pom) {
        this(null, model, pom);
    }

    /**
     * Creates a new instance with a parent node.
     *
     * @param parent parent node
     * @param model {@link Model} instance
     * @param pom {@link ModifiedPomXMLEventReader} instance
     */
    public ModelNode(ModelNode parent, Model model, ModifiedPomXMLEventReader pom) {
        this.parent = parent;
        this.item = model;
        this.pom = pom;
    }

    /**
     * Returns the {@link Model} instance associated with the given node.
     *
     * @return {@link Model} associated with the given node, never {@code null}.
     */
    public Model getModel() {
        return item;
    }

    /**
     * Gets the parent node.
     *
     * @return The parent node of this node or {@link Optional#empty()}, never {@code null}.
     */
    public Optional<ModelNode> getParent() {
        return Optional.ofNullable(parent);
    }

    /**
     * Gets the {@link ModifiedPomXMLEventReader} instance
     *
     * @return the {@link ModifiedPomXMLEventReader} instance
     */
    public ModifiedPomXMLEventReader getModifiedPomXMLEventReader() {
        return pom;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof ModelNode) {
            ModelNode other = (ModelNode) o;
            return item.equals(other.item) && (parent == null || parent.equals(other.parent)) && pom.equals(other.pom);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 13 * item.hashCode() + (parent == null ? 0 : 23 * parent.hashCode()) + 7 * pom.hashCode();
    }
}
