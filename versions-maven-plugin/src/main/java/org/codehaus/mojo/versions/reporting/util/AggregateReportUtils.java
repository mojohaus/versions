package org.codehaus.mojo.versions.reporting.util;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.maven.project.MavenProject;

/**
 * Class that contains common aggregate operations
 *
 * @since 2.14.0
 * */
public class AggregateReportUtils
{

    /**
     * Returns an aggregated list of {@link MavenProject} for the given project.
     *
     * @param project to aggregate
     *
     * @return aggregated list of MavenProject objects for the given project(also containing the project itself)
     * */
    public static List<MavenProject> getProjectsToProcess( final MavenProject project )
    {
        if ( project == null )
        {
            return Collections.emptyList();
        }

        List<MavenProject> result = new ArrayList<>();
        result.add( project );
        result.addAll( project.getCollectedProjects() );
        return result;
    }

}
