package org.codehaus.mojo.versions;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Updates components of the project's version if it follows the maven-semver format of &lt;major&gt;[.&lt;minor&gt;[.&lt;patch&gt;]][&lt;qualifier&gt;][-SNAPSHOT]<p> 
 * Major, minor and patch versions are zero or positive integers.<p>
 * Qualifier is any sequence of characters valid in a Maven version.<p>
 * -SNAPSHOT can be present or absent is not considered part of the qualifier. <p><p>
 *
 * Sample valid maven-semver version numbers :<p>
 * 0 (major 0, no qualifier, snapshot false)<p>
 * 0.2-SNAPSHOT (major 0, minor 2, no qualifier, snapshot true)<p>
 * 2.0.6.RELEASE (major 2, minor 0, patch 6, qualifier ".RELEASE", snapshot false)<p><p>
 *  
 * <p><p>
 * 
 * New version string can be set at once with the `newVersion` parameter. <p>
 * Individual version numbers can be set to absolute values by one with the "setMajorVersion", "setMinorVersion", "setPatchVersion" parameters. <p>
 * Existing individual version numbers can be incremented by one with the "incrementMajorVersion", "incrementMinorVersion", "incrementPatchVersion" parameters. <p>
 * Snapshot state can be set to an absolute boolean value with the "setSnapshot"parameter. <p>
 * Qualifier can be set to any absolute string value with the "setQualifier" parameter. <p><p>
 *    
 * Minor and patch versions numbers can be added to a version if it did not have them before. <p>
 * Minor and patch versions numbers can not be _removed_ from a version once set. <p>
 *
 * @author Francis Lalonde
 * @since 2.4.0
 */
@Mojo(name = "semver", requiresProject = true, requiresDirectInvocation = true, aggregator = true)
public class SemverMojo 
	extends AbstractModuleVersionUpdaterMojo 
{

	private static final Pattern VERSION_PATTERN = Pattern.compile("0|[1-9]\\d*");

	/**
	 * The integral new semver version to set.
	 * Version must be of format &lt;major&gt;[.&lt;minor&gt;[.&lt;patch&gt;]][&lt;qualifier&gt;][-SNAPSHOT]
	 * For example, 2.4.6-alpha-SNAPSHOT
	 * Components of this string can be altered by other parameters of the semver goal. 
	 * If not specified, the project's existing version will be used as a starting point.
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "newVersion") String newVersion;

	/**
	 * If set to true, forces project version to end with -SNAPSHOT.
	 * If set to false, removes the -SNAPSHOT at the end of the version if there was one. 
	 * If left null (default), existing snapshot status is preserved.
	 * 
	 * @since 2.4.0
	 */
	@Parameter(property = "setSnapshot")
	private Boolean setSnapshot;

	/**
	 * If set, updates the version qualifier.
	 * If left null (default), existing qualifier is preserved.
	 * 
	 * @since 2.4.0
	 */
	@Parameter(property = "setQualifier")
	private String setQualifier;
	
	/**
	 * Increment the major version by 1. 
	 * For example, version "2.4.6" will be changed to "3.4.6".
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "incrementMajorVersion", defaultValue = "false")
	private boolean incrementMajorVersion;

	/**
	 * Increment the minor version by 1. 
	 * For example, version "2.4.6" will be changed to "2.5.6".
	 * Existing version needs to already have a minor version set or the operation will fail. 
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "incrementMinorVersion", defaultValue = "false")
	private boolean incrementMinorVersion;

	/**
	 * Increment the patch version by 1. 
	 * For example, version "2.4.6" will be changed to "2.4.7".
	 * Existing version needs to already have a patch version set or the operation will fail. 
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "incrementPatchVersion", defaultValue = "false")
	private boolean incrementPatchVersion;

	/**
	 * If set, major version will forcefully be set to the the value. 
	 * If both this parameter and incrementMajorVersion are set, incrementMajorVersion will be ignored.
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "setMajorVersion")
	private Integer setMajorVersion;

	/**
	 * If set, minor version will forcefully be set to the the value. 
	 * If both this parameter and incrementMinorVersion are set, incrementMinorVersion will be ignored.
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "setMinorVersion")
	private Integer setMinorVersion;

	/**
	 * If set, patch version will forcefully be set to the the value. 
	 * If both this parameter and incrementPatchVersion are set, incrementPatchVersion will be ignored.
	 *
	 * @since 2.4.0
	 */
	@Parameter(property = "setPatchVersion")
	private Integer setPatchVersion;
	
	private static class MavenSemver 
	{
		int major;
		Integer minor;
		Integer patch;
		String qualifier;
		boolean snapshot;

		static MavenSemver from(String version) throws MojoExecutionException 
		{
			MavenSemver semver = new MavenSemver();
			semver.snapshot = version.endsWith(SNAPSHOT);
			if (semver.snapshot) {
				version = version.substring(0, version.length() - SNAPSHOT.length());
			}
			
			Matcher matcher = VERSION_PATTERN.matcher(version);
			if (!matcher.find()) 
			{
				throw new MojoExecutionException("No major version could be parsed, version does not follow Maven semver format <major>[.<minor>[.<patch>]][<qualifier>][-SNAPSHOT]");
			}
			
			semver.major = Integer.parseInt(matcher.group());
			int qualifierStart = matcher.end();

			if (matcher.find()) 
			{
				semver.minor = Integer.parseInt(matcher.group());
				qualifierStart = matcher.end();
				
				// can only have patch version if minor version is also set 
				if (matcher.find()) 
				{
					semver.patch = Integer.parseInt(matcher.group());
					qualifierStart = matcher.end();
				}
			}

			semver.qualifier = version.substring(qualifierStart);
			return semver;
		}
		
		public String toString() {
			return String.format("major(%d) minor(%d) patch(%d) qualifier(%s) snapshot(%b)", major, minor, patch, qualifier, snapshot);
 
		}

		public String toVersionString() {
			StringBuilder sb = new StringBuilder();
			sb.append(major);
			if (minor != null) 
			{
				sb.append(".").append(minor);
				
				// can only have patch version if minor version is also set 
				if (patch != null) 
				{
					sb.append(".").append(patch);
				}
			}
			if (qualifier != null) 
			{
				sb.append(qualifier);
			}
			if (snapshot) 
			{
				sb.append(SNAPSHOT);
			}
			return sb.toString();
		}
	}

	/**
	 * Called when this mojo is executed.
	 *
	 * @throws org.apache.maven.plugin.MojoExecutionException
	 *             when things go wrong.
	 * @throws org.apache.maven.plugin.MojoFailureException
	 *             when things go wrong.
	 */
	public void execute() throws MojoExecutionException, MojoFailureException {

		if (getProject().getOriginalModel().getVersion() == null) 
		{
			throw new MojoExecutionException("Project version is inherited from parent.");
		}

		MavenSemver semver = MavenSemver.from(newVersion != null ? newVersion : getVersion());
		getLog().info("Parsed original semver " + semver);		
		
		if (setSnapshot != null) 
		{
			semver.snapshot = setSnapshot;
		}		

		if (setQualifier != null) 
		{
			semver.qualifier = setQualifier;
		}		

		if (setMajorVersion != null) 
		{
			semver.major = setMajorVersion;
		}
		else if (incrementMajorVersion) 
		{
			semver.major++;
		}

		if (setMinorVersion != null) 
		{
			semver.minor = setMinorVersion;
		}
		else if (incrementMinorVersion) 
		{
			if (semver.minor == null) 
			{
				throw new MojoExecutionException("No minor version to increment.");
			}
			semver.minor++;
		}

		if (setPatchVersion != null) 
		{
			if (semver.minor == null) 
			{
				throw new MojoExecutionException("Cannot set patch version without minor version.");
			}
			semver.patch = setPatchVersion;
		}
		else if (incrementPatchVersion) 
		{
			if (semver.minor == null) 
			{
				throw new MojoExecutionException("No patch version to increment.");
			}
			semver.patch++;
		}

		setVersion(semver.toVersionString());
	}

}
