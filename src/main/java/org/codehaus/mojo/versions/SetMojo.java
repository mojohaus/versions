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

import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.codehaus.plexus.util.StringUtils;

/**
 * Sets the current project's version and based on that change propagates that change onto any child modules as necessary.
 *
 * @author Stephen Connolly
 * @goal set
 * @aggregator
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-beta-1
 */
public class SetMojo
        extends AbstractVersionsSetMojo {

    /**
     * The new version number to set.
     *
     * @parameter property="newVersion"
     * @since 1.0-beta-1
     */
    private String newVersion;


    /**
     * Component used to prompt for input
     *
     * @component
     */
    private Prompter prompter;

    @Override
    protected String getNewVersion() throws MojoExecutionException {
        if (StringUtils.isEmpty(newVersion)) {
            if (settings.isInteractiveMode()) {
                try {
                    newVersion =
                            prompter.prompt("Enter the new version to set", getProject().getOriginalModel().getVersion());
                } catch (PrompterException e) {
                    throw new MojoExecutionException(e.getMessage(), e);
                }
            } else {
                throw new MojoExecutionException("You must specify the new version, either by using the newVersion "
                        + "property (that is -DnewVersion=... on the command line) or run in interactive mode");
            }
        }

        return newVersion;
    }
}

