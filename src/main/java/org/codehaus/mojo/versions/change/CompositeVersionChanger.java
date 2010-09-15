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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import javax.xml.stream.XMLStreamException;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 16:01:35
 */
public class CompositeVersionChanger
    implements VersionChanger
{
    private final List/*<VersionChanger>*/ composites;

    public CompositeVersionChanger( VersionChanger[] composites )
    {
        this.composites = new ArrayList( Arrays.asList( composites ) );
    }

    public CompositeVersionChanger( List/*<VersionChanger>*/ composites )
    {
        this.composites = new ArrayList( composites );
    }

    public void apply( VersionChange versionChange )
        throws XMLStreamException
    {
        Iterator/*<VersionChanger>*/ i = composites.iterator();
        while ( i.hasNext() )
        {
            VersionChanger delegate = (VersionChanger) i.next();
            delegate.apply( versionChange );
        }
    }
}
