package org.codehaus.mojo.versions.api;

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

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.io.StreamCorruptedException;
import java.util.HashMap;
import java.util.Map;

/**
 * Scopes of version updates.
 *
 * @author connollys
 * @since 1.0-beta-1
 */
public abstract class UpdateScope
    implements Comparable, Serializable
{

    /**
     * Versions which are less than an incremental update.
     *
     * @since 1.0-beta-1
     */
    public static final UpdateScope SUBINCREMENTAL = new UpdateScope( "SUBINCREMENTAL", 0 )
    {
        /** {@inheritDoc} */
        public ArtifactVersion getNext( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                        boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 3
                ? null
                : versionDetails.getOldestVersion( currentVersion,
                                                   versionComparator.incrementSegment( currentVersion, 2 ),
                                                   includeSnapshots, false, false );
        }

        /** {@inheritDoc} */
        public ArtifactVersion getLatest( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                          boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 3
                ? null
                : versionDetails.getLatestVersion( currentVersion,
                                                   versionComparator.incrementSegment( currentVersion, 2 ),
                                                   includeSnapshots, false, false );
        }

    };

    /**
     * Incremental version updates, that is the third segment of the version number, for example <code>1.0.0.15</code>
     * to <code>1.0.1.0</code>.
     *
     * @since 1.0-beta-1
     */
    public static final UpdateScope INCREMENTAL = new UpdateScope( "INCREMENTAL", 1 )
    {
        /** {@inheritDoc} */
        public ArtifactVersion getNext( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                        boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 3
                ? null
                : versionDetails.getOldestVersion( versionComparator.incrementSegment( currentVersion, 2 ),
                                                   versionComparator.incrementSegment( currentVersion, 1 ),
                                                   includeSnapshots, true, false );
        }

        /** {@inheritDoc} */
        public ArtifactVersion getLatest( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                          boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 3
                ? null
                : versionDetails.getLatestVersion( versionComparator.incrementSegment( currentVersion, 2 ),
                                                   versionComparator.incrementSegment( currentVersion, 1 ),
                                                   includeSnapshots, true, false );
        }

    };

    /**
     * Minor version updates, that is the second segment of the version number, for example <code>1.0.0.15</code>
     * to <code>1.1.0.0</code>.
     *
     * @since 1.0-beta-1
     */
    public static final UpdateScope MINOR = new UpdateScope( "MINOR", 2 )
    {
        /** {@inheritDoc} */
        public ArtifactVersion getNext( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                        boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 2
                ? null
                : versionDetails.getOldestVersion( versionComparator.incrementSegment( currentVersion, 1 ),
                                                   versionComparator.incrementSegment( currentVersion, 0 ),
                                                   includeSnapshots, true, false );
        }

        /** {@inheritDoc} */
        public ArtifactVersion getLatest( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                          boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 2
                ? null
                : versionDetails.getLatestVersion( versionComparator.incrementSegment( currentVersion, 1 ),
                                                   versionComparator.incrementSegment( currentVersion, 0 ),
                                                   includeSnapshots, true, false );
        }

    };

    /**
     * Major version updates, that is the first segment of the version number, for example <code>1.0.0.15</code>
     * to <code>2.0.0.0</code>.
     *
     * @since 1.0-beta-1
     */
    public static final UpdateScope MAJOR = new UpdateScope( "MAJOR", 3 )
    {
        /** {@inheritDoc} */
        public ArtifactVersion getNext( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                        boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 1
                ? null
                : versionDetails.getOldestVersion( versionComparator.incrementSegment( currentVersion, 0 ), null,
                                                   includeSnapshots, true, false );
        }

        /** {@inheritDoc} */
        public ArtifactVersion getLatest( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                          boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            VersionComparator versionComparator = versionDetails.getVersionComparator();
            return versionComparator.getSegmentCount( currentVersion ) < 1
                ? null
                : versionDetails.getLatestVersion( versionComparator.incrementSegment( currentVersion, 0 ), null,
                                                   includeSnapshots, true, false );
        }

    };

    /**
     * Any version updates.
     *
     * @since 1.0-beta-1
     */
    public static final UpdateScope ANY = new UpdateScope( "ANY", 4 )
    {
        /** {@inheritDoc} */
        public ArtifactVersion getNext( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                        boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            return versionDetails.getOldestVersion( currentVersion, null, includeSnapshots, false, false );
        }

        /** {@inheritDoc} */
        public ArtifactVersion getLatest( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                          boolean includeSnapshots )
            throws ArtifactMetadataRetrievalException
        {
            return versionDetails.getLatestVersion( currentVersion, null, includeSnapshots, false, false );
        }

    };

    /**
     * Returns the next version after the specified current version within this scope.
     *
     * @param versionDetails   The versions to select from.
     * @param currentVersion   The current version.
     * @param includeSnapshots Whether to include snapshots.
     * @return The next version within this scope or <code>null</code> if there is no version within this scope.
     * @throws ArtifactMetadataRetrievalException
     *          if there was a problem retrieving the list of available versions.
     */
    public abstract ArtifactVersion getNext( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                             boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the newest version after the specified current version within this scope.
     *
     * @param versionDetails   The versions to select from.
     * @param currentVersion   The current version.
     * @param includeSnapshots Whether to include snapshots.
     * @return The newest version within this scope or <code>null</code> if there is no version within this scope.
     * @throws ArtifactMetadataRetrievalException
     *          if there was a problem retrieving the list of available versions.
     */
    public abstract ArtifactVersion getLatest( VersionDetails versionDetails, ArtifactVersion currentVersion,
                                               boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * The name of this constant, as declared in the declaration. Most programmers should use the {@link #toString}
     * method rather than accessing this field.
     */
    private final String name;

    /**
     * Returns the name of this enum constant, exactly as declared in its enum declaration.
     * <p/>
     * <b>Most programmers should use the {@link #toString} method in preference to this one, as the toString method may
     * return a more user-friendly name.</b>  This method is designed primarily for use in specialized situations where
     * correctness depends on getting the exact name, which will not vary from release to release.
     *
     * @return the name of this enum constant
     */
    public final String name()
    {
        return name;
    }

    /**
     * The ordinal of this enumeration constant (its position in the enum declaration, where the initial constant is
     * assigned an ordinal of zero).
     * <p/>
     * Most programmers will have no use for this field.
     */
    private final int ordinal;

    /**
     * Returns the ordinal of this enumeration constant (its position in its enum declaration, where the initial
     * constant is assigned an ordinal of zero).
     * <p/>
     * Most programmers will have no use for this method.
     *
     * @return the ordinal of this enumeration constant
     */
    public final int ordinal()
    {
        return ordinal;
    }

    /**
     * Sole constructor.  Programmers cannot invoke this constructor.
     *
     * @param name    - The name of this enum constant, which is the identifier used to declare it.
     * @param ordinal - The ordinal of this enumeration constant (its position in the enum declaration, where the
     *                initial constant is assigned an ordinal of zero).
     */
    private UpdateScope( String name, int ordinal )
    {
        this.name = name;
        this.ordinal = ordinal;
    }

    /**
     * {@inheritDoc}
     */
    public String toString()
    {
        return name;
    }

    /**
     * {@inheritDoc}
     */
    public boolean equals( Object o )
    {
        return this == o;
    }

    /**
     * {@inheritDoc}
     */
    public int hashCode()
    {
        return super.hashCode();
    }

    /**
     * Throws CloneNotSupportedException.  This guarantees that levels are never cloned, which is necessary to preserve
     * their "singleton" status.
     *
     * @return (neverreturns)
     */
    protected final Object clone()
        throws CloneNotSupportedException
    {
        throw new CloneNotSupportedException();
    }

    /**
     * Compares this enum with the specified object for order.  Returns a negative integer, zero, or a positive integer
     * as this object is less than, equal to, or greater than the specified object.
     * <p/>
     * Enum constants are only comparable to other enum constants of the same enum type.  The natural order implemented
     * by this method is the order in which the constants are declared.
     */
    public final int compareTo( Object o )
    {
        UpdateScope other = (UpdateScope) o;
        UpdateScope self = this;
        if ( self.getClass() != other.getClass() )
        {
            throw new ClassCastException();
        }
        return self.ordinal - other.ordinal;
    }

    /**
     * Returns the Class object corresponding to this enum constant's enum type.  Two enum constants e1 and  e2 are of
     * the same enum type if and only if e1.getDeclaringClass() == e2.getDeclaringClass(). (The value returned by this
     * method may differ from the one returned by the {@link Object#getClass} method for enum constants with
     * constant-specific class bodies.)
     *
     * @return the Class object corresponding to this enum constant's enum type
     */
    public final Class getDeclaringClass()
    {
        return getClass();
    }

    /**
     * Returns the enum constant of the specified enum type with the specified name.  The name must match exactly an
     * identifier used to declare an enum constant in this type.  (Extraneous whitespace characters are not permitted.)
     *
     * @param name the name of the constant to return
     * @return the enum constant of the specified enum type with the specified name
     * @throws IllegalArgumentException if the specified enum type has no constant with the specified name, or the
     *                                  specified class object does not represent an enum type
     * @throws NullPointerException     if <tt>enumType</tt> or <tt>name</tt> is null
     */
    public static UpdateScope valueOf( String name )
    {
        UpdateScope result = (UpdateScope) levelConstants.get( name );
        if ( result != null )
        {
            return result;
        }
        if ( name == null )
        {
            throw new NullPointerException( "Name is null" );
        }
        throw new IllegalArgumentException( "No enum const " + UpdateScope.class.getName() + "." + name );
    }

    public static UpdateScope[] values()
    {
        return new UpdateScope[]{SUBINCREMENTAL, INCREMENTAL, MINOR, MAJOR};
    }

    private static final Map levelConstants;

    static
    {
        Map map = new HashMap( 4 );
        map.put( SUBINCREMENTAL.name(), SUBINCREMENTAL );
        map.put( INCREMENTAL.name(), INCREMENTAL );
        map.put( MINOR.name(), MINOR );
        map.put( MAJOR.name(), MAJOR );
        levelConstants = map;
    }

    /**
     * enum classes cannot have finalize methods.
     */
    protected final void finalize()
        throws Throwable
    {
        super.finalize();
    }

    /**
     * We need to ensure that the singleton is used when deserializing.
     *
     * @return The correct singleton instance.
     * @throws java.io.ObjectStreamException when things go wrong.
     */
    private Object readResolve()
        throws ObjectStreamException
    {
        if ( ordinal == SUBINCREMENTAL.ordinal )
        {
            return SUBINCREMENTAL;
        }
        if ( ordinal == INCREMENTAL.ordinal )
        {
            return INCREMENTAL;
        }
        if ( ordinal == MINOR.ordinal )
        {
            return MINOR;
        }
        if ( ordinal == MAJOR.ordinal )
        {
            return MAJOR;
        }
        throw new StreamCorruptedException();
    }

}
