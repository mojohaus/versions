package org.codehaus.mojo.versions.reporting;


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

/**
 * Represents summary stats
 *
 * @author Andrzej Jarmoniuk
 */
class OverviewStats
{
    private int major;

    private int minor;

    private int incremental;

    private int any;

    private int upToDate;

    public int getMajor()
    {
        return major;
    }

    public void incrementMajor()
    {
        major++;
    }

    public int getMinor()
    {
        return minor;
    }

    public void incrementMinor()
    {
        minor++;
    }

    public int getIncremental()
    {
        return incremental;
    }

    public void incrementIncremental()
    {
        incremental++;
    }

    public int getAny()
    {
        return any;
    }

    public void incrementAny()
    {
        any++;
    }

    public int getUpToDate()
    {
        return upToDate;
    }

    public void incrementUpToDate()
    {
        upToDate++;
    }
}
