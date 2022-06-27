package org.codehaus.mojo.versions;

import org.apache.maven.model.Dependency;

public class DependencyBuilder
{

    public String getGroupId()
    {
        return groupId;
    }

    public DependencyBuilder withGroupId( String groupId )
    {
        this.groupId = groupId;
        return this;
    }

    public String getArtifactId()
    {
        return artifactId;
    }

    public DependencyBuilder withArtifactId( String artifactId )
    {
        this.artifactId = artifactId;
        return this;
    }

    public String getVersion()
    {
        return version;
    }

    public DependencyBuilder withVersion( String version )
    {
        this.version = version;
        return this;
    }

    public String getType()
    {
        return type;
    }

    public DependencyBuilder withType( String type )
    {
        this.type = type;
        return this;
    }

    public String getClassifier()
    {
        return classifier;
    }

    public DependencyBuilder withClassifier( String classifier )
    {
        this.classifier = classifier;
        return this;
    }

    public String getScope()
    {
        return scope;
    }

    public DependencyBuilder withScope( String scope )
    {
        this.scope = scope;
        return this;
    }

    private String groupId = null;
    private String artifactId = null;
    private String version = null;
    private String type = null;
    private String classifier = null;
    private String scope = null;

    public static DependencyBuilder newBuilder()
    {
        return new DependencyBuilder();
    }

    public static DependencyBuilder newBuilder( String groupId, String artifactId, String version, String type,
                                                String classifier, String scope )
    {
        return newBuilder()
                .withGroupId( groupId )
                .withArtifactId( artifactId )
                .withVersion( version )
                .withType( type )
                .withClassifier( classifier )
                .withScope( scope );
    }

    public static DependencyBuilder newBuilder( String groupId, String artifactId, String version )
    {
        return newBuilder()
                .withGroupId( groupId )
                .withArtifactId( artifactId )
                .withVersion( version );
    }

    public static Dependency dependencyWith( String groupId, String artifactId, String version )
    {
        return newBuilder( groupId, artifactId, version )
                .build();
    }

    public static Dependency dependencyWith( String groupId, String artifactId, String version, String type,
                                             String classifier, String scope )
    {
        return newBuilder( groupId, artifactId, version, type, classifier, scope )
                .build();
    }


    public Dependency build()
    {
        Dependency dep = new Dependency();
        dep.setGroupId( groupId );
        dep.setArtifactId( artifactId );
        dep.setVersion( version );
        dep.setType( type );
        dep.setClassifier( classifier );
        dep.setScope( scope );

        return dep;
    }

}
