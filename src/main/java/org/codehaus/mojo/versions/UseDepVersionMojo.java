package org.codehaus.mojo.versions;

import java.util.Collection;
import java.util.Iterator;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * 
 * @author Dan Arcari
 * @goal use-dep-version
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 2.3
 */
public class UseDepVersionMojo extends AbstractVersionsDependencyUpdaterMojo {

	/**
	 * The exact version to be applied for the included dependencies
	 * @parameter property="depVersion" required="true"
	 */
	protected String depVersion;

	/**
	 * If set to true, will use whatever version is supplied without attempting to validate that such a version is obtainable from the repository chain.
	 * @parameter property="forceVersion" required="false" default-value="false"
	 */
	protected boolean forceVersion;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void update(ModifiedPomXMLEventReader pom) throws MojoExecutionException, MojoFailureException,
			XMLStreamException, ArtifactMetadataRetrievalException {
		
		if (depVersion == null || depVersion.equals("")) {
			throw new IllegalArgumentException("depVersion must be supplied with use-specific-version, and cannot be blank.");
		}
		
		if (!forceVersion && !hasIncludes()) {
			throw new IllegalArgumentException("The use-specific-version goal is intended to be used with a single artifact. Please specify a value for the 'includes' parameter, or use -DforceVersion=true to override this check.");
		}
		
		try {
            if (getProject().getDependencyManagement() != null && isProcessingDependencyManagement()) {
            	useDepVersion(pom, getProject().getDependencyManagement().getDependencies());
            }
            
            if (isProcessingDependencies()) {
            	useDepVersion(pom, getProject().getDependencies());
            }
        }
        catch (ArtifactMetadataRetrievalException e) {
            throw new MojoExecutionException( e.getMessage(), e );
        }
	}

	private void useDepVersion(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies) 
			throws MojoExecutionException, XMLStreamException, ArtifactMetadataRetrievalException {
		Iterator<Dependency> itr = dependencies.iterator();
		
		while (itr.hasNext()) {
			Dependency dep = (Dependency)itr.next();
			
			if (isExcludeReactor() && isProducedByReactor(dep)) {
                getLog().info( "Ignoring reactor dependency: " + toString(dep));
                continue;
            }
			
			Artifact artifact = this.toArtifact(dep);

			if (isIncluded(artifact)) {
				if (!forceVersion) {
					ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, false);
					
					if (!versions.containsVersion(depVersion)) {
						throw new MojoExecutionException(String.format("Version %s is not available for artifact %s:%s", 
								depVersion, artifact.getGroupId(), artifact.getArtifactId()));
					}
				}
				
				String version = dep.getVersion();
				
				if (PomHelper.setDependencyVersion(pom, dep.getGroupId(), dep.getArtifactId(), version, depVersion)) {
					getLog().info("Updated " + toString(dep) + " to version " + depVersion);
				}
			}
		}
	}
}