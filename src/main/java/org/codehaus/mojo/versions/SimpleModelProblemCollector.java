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
 *  http://www.apache.org/licenses/LICENSE-2.0
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

import org.apache.maven.model.InputLocation;
import org.apache.maven.model.building.ModelProblem.Severity;
import org.apache.maven.model.building.ModelProblemCollector;

/**
 * A simple model problem collector for testing the model building components.
 * 
 * @author Benjamin Bentmann
 */
class SimpleModelProblemCollector
    implements ModelProblemCollector
{

    private List<String> warnings = new ArrayList<String>();

    private List<String> errors = new ArrayList<String>();

    private List<String> fatals = new ArrayList<String>();

    public List<String> getWarnings()
    {
        return warnings;
    }

    public List<String> getErrors()
    {
        return errors;
    }

    public List<String> getFatals()
    {
        return fatals;
    }

    public void add( Severity severity, String message, InputLocation location, Exception cause )
    {
        switch ( severity )
        {
            case FATAL:
                fatals.add( message );
                break;
            case ERROR:
                errors.add( message );
                break;
            case WARNING:
                warnings.add( message );
                break;
        }

    }

}