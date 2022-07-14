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

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.path.PathTranslator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 17-Mar-2009 08:51:42
 */
public class VersionsExpressionEvaluator
    extends PluginParameterExpressionEvaluator
    implements ExpressionEvaluator
{
    public VersionsExpressionEvaluator( MavenSession mavenSession, PathTranslator pathTranslator,
                                        MavenProject mavenProject )
    {
        super( mavenSession, new MojoExecution( new MojoDescriptor() ), pathTranslator, null, mavenProject,
               mavenSession.getExecutionProperties() );
    }
}
