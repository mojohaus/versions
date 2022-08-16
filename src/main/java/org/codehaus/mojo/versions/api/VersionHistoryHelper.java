package org.codehaus.mojo.versions.api;

import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.plugin.logging.Log;

import com.corgibytes.maven.ReleaseHistoryService;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class VersionHistoryHelper {
    private List<ReleaseHistoryService> releaseHistoryServices;

    public VersionHistoryHelper(List<ArtifactRepository> remoteArtifactRepositories, List<ArtifactRepository> remotePluginRepositories, Log log)
    {
        releaseHistoryServices = new ArrayList<ReleaseHistoryService>();

        List<ArtifactRepository> consolidatedRepositoryList = consolidateRepositories(remoteArtifactRepositories, remotePluginRepositories);
        this.releaseHistoryServices = buildReleaseHistoryServices(consolidatedRepositoryList);
    }

    public ArtifactVersions filterVersions(ArtifactVersions artifactVersions, ZonedDateTime versionsAsOf, boolean includeSnapshots)
    {
        Map<String, ZonedDateTime> releaseHistory = getReleaseHistory(artifactVersions.getGroupId(), artifactVersions.getArtifactId());

        List<ArtifactVersion> versions = new ArrayList<>();
        for (ArtifactVersion candidateVersion : artifactVersions.getVersions(includeSnapshots))
        {
            if (releaseHistory.containsKey(candidateVersion.toString()))
            {
                ZonedDateTime releaseDate = releaseHistory.get(candidateVersion.toString());
                if (releaseDate.isBefore(versionsAsOf) || releaseDate.equals(versionsAsOf))
                {
                    versions.add(candidateVersion);
                }
            }
        }
        return new ArtifactVersions(artifactVersions.getArtifact(), versions, artifactVersions.getVersionComparator());
    }

    private Map<String, ZonedDateTime> getReleaseHistory(String groupId, String artifactId)
    {
        HashMap<String, ZonedDateTime> results = new HashMap<>();

        for (ReleaseHistoryService service : releaseHistoryServices)
        {
            Map<String, ZonedDateTime> history = service.getVersionHistory(groupId, artifactId);
            for (String version : history.keySet())
            {
                if (!results.containsKey(version))
                {
                    results.put(version, history.get(version));
                }
            }
        }

        return results;
    }

    private List<ArtifactRepository> consolidateRepositories(List<ArtifactRepository>... repositoryLists)
    {
        List<ArtifactRepository> results = new ArrayList<>();

        for (List<ArtifactRepository> repositoryList : repositoryLists)
        {
            for (ArtifactRepository repository : repositoryList)
            {
                results.add(repository);
            }
        }

        return results.stream().distinct().collect(Collectors.toList());
    }

    private List<ReleaseHistoryService> buildReleaseHistoryServices(List<ArtifactRepository> repositoryList)
    {
        List<ReleaseHistoryService> results = new ArrayList<>();

        for (ArtifactRepository repository : repositoryList)
        {
            results.add(buildReleaseHistoryService(repository));
        }

        return results;
    }

    private ReleaseHistoryService buildReleaseHistoryService(ArtifactRepository repository)
    {
        ReleaseHistoryService service = new ReleaseHistoryService(repository.getUrl());
        return service;
    }
}
