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
*  http://www.apache.org/licenses/LICENSE-2.0
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
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 16:05:27
 */
public class VersionChangerFactory
{
    private Model model = null;

    private ModifiedPomXMLEventReader pom = null;

    private Log log = null;

    public synchronized Model getModel()
    {
        return model;
    }

    public synchronized void setModel( Model model )
    {
        this.model = model;
    }

    public synchronized ModifiedPomXMLEventReader getPom()
    {
        return pom;
    }

    public synchronized void setPom( ModifiedPomXMLEventReader pom )
    {
        this.pom = pom;
    }

    public synchronized Log getLog()
    {
        return log;
    }

    public synchronized void setLog( Log log )
    {
        this.log = log;
    }

    private synchronized void checkState()
    {
        if ( model == null )
        {
            throw new IllegalStateException( "Model has not been specified" );
        }
        if ( pom == null )
        {
            throw new IllegalStateException( "Pom has not been specified" );
        }
        if ( log == null )
        {
            throw new IllegalStateException( "Log has not been specified" );
        }
    }

    public synchronized VersionChanger newPluginVersionChanger()
    {
        checkState();
        return new PluginVersionChanger( model, pom, log );
    }

    public synchronized VersionChanger newDependencyVersionChanger()
    {
        checkState();
        return new DependencyVersionChanger( model, pom, log );
    }

    public synchronized VersionChanger newProjectVersionChanger()
    {
        checkState();
        return new ProjectVersionChanger( model, pom, log );
    }

    public synchronized VersionChanger newVersionChanger()
    {
        checkState();
        VersionChanger[] delegates =
            new VersionChanger[]{newProjectVersionChanger(), newDependencyVersionChanger(), newPluginVersionChanger()};
        return new CompositeVersionChanger( delegates );
    }
}
