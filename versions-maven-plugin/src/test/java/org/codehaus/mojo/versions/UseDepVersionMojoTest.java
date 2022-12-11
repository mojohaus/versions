package org.codehaus.mojo.versions;

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

import java.io.File;
import java.util.Collections;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.junit.Rule;
import org.junit.Test;

/**
 * Basic tests for {@linkplain UseDepVersionMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class UseDepVersionMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    @Test
    public void testIssue673() throws Exception {
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(
                new File("target/test-classes/org/codehaus/mojo/use-dep-version/issue-637"), "use-dep-version");
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "processDependencyManagement", true);
        setVariableValueToObject(mojo, "excludeReactor", true);
        setVariableValueToObject(mojo, "serverId", "serverId");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));

        mojo.execute();
    }
}
