package org.codehaus.mojo.versions.change;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.xml.stream.XMLStreamException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.codehaus.mojo.versions.api.change.DependencyVersionChange;

/**
 * A composite version changer, which chains version changers together and executes the chain
 * in {@link #apply(DependencyVersionChange)} until an exception occurs or the chain is through.
 */
public class CompositeVersionChanger implements VersionChanger {
    private final List<VersionChanger> composites;

    /**
     * Construct a new composite version changed based on an array of version changers
     * @param composites array of version changes to compose from
     */
    public CompositeVersionChanger(VersionChanger[] composites) {
        this.composites = new ArrayList<>(Arrays.asList(composites));
    }

    /**
     * Construct a new composite version changed based on a {@link List} of version changers
     * @param composites list of version changes to compose from
     */
    public CompositeVersionChanger(List<VersionChanger> composites) {
        this.composites = new ArrayList<>(composites);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void apply(DependencyVersionChange versionChange) throws XMLStreamException {
        for (VersionChanger delegate : composites) {
            delegate.apply(versionChange);
        }
    }
}
