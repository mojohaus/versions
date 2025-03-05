package org.codehaus.mojo.versions.ordering;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import javax.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;

/**
 * <p>Represents an <b>immutable</b> artifact version with all segments <em>major</em> to the given segment
 * held in place. It can be thought of as an artifact having +∞ as its upper bound
 * on all segments minor to the held segment.</p>
 * <p>For example:</p>
 * <p>A {@link BoundArtifactVersion} of {@code [1.2.3-2, INCREMENTAL]} can be seen as {@code 1.2.+∞}
 * and will be greater than all versions matching the {@code 1.2.*} pattern.</p>
 * <p>A {@link BoundArtifactVersion} of {@code [1.2.3-2, SUBINCREMENTAL]} will be greater
 *  * than all versions matching the {@code 1.2.3-2.*} pattern.</p>
 * <p>When compared to another artifact versions, this results with the other object
 * with the segment versions up to the held segment being equal,
 * always comparing lower than this object.</p>
 * <p>This is particularly helpful for -SNAPSHOT and other versions with qualifiers, which
 * are lower than version 0 in the Maven versioning system.</p>
 */
public class BoundArtifactVersion implements ArtifactVersion {
    /**
     * Most major segment that can change, i.e. not held in place.
     * All segments that are more major than this one are held in place.
     */
    private final Segment segment;

    private final ArtifactVersion comparable;

    /**
     * Constructs the instance given the version in a text format.
     * @param artifactVersion lower bound
     * @param segment most major segment that can change, i.e. <em>not</em> held in place
     */
    public BoundArtifactVersion(String artifactVersion, Segment segment) {
        this.segment = segment;
        StringBuilder versionBuilder = new StringBuilder();
        String[] segments = tokens(artifactVersion);
        for (int segNr = 0;
                segNr <= segments.length || segNr <= Segment.SUBINCREMENTAL.value();
                segNr++, versionBuilder.append(".")) {
            if (segNr < segment.value()) {
                versionBuilder.append(segNr < segments.length ? integerItemOrZero(segments[segNr]) : "0");
            } else {
                versionBuilder.append(Integer.MAX_VALUE);
            }
        }
        versionBuilder.append(Integer.MAX_VALUE);
        comparable = ArtifactVersionService.getArtifactVersion(versionBuilder.toString());
    }

    /**
     * Constructs the instance given a {@link ArtifactVersion instance}
     * @param artifactVersion lower bound
     * @param segment most major segment that can change, i.e. <em>not</em> held in place
     */
    public BoundArtifactVersion(ArtifactVersion artifactVersion, Segment segment) {
        this(artifactVersion.toString(), segment);
    }

    /**
     * Splits the given version string into tokens, splitting them on the {@code .} or {@code -} characters
     * as well as on letter/digit boundaries.
     * @param version version string
     * @return tokens of the parsed version string
     */
    private static String[] tokens(String version) {
        if (version == null) {
            return new String[0];
        }
        List<String> result = new ArrayList<>();
        for (int begin = 0, end = 0; end <= version.length(); end++) {
            if (end == version.length()
                    || version.charAt(end) == '.'
                    || version.charAt(end) == '-'
                    || isTokenBoundary(version.charAt(begin), version.charAt(end))) {
                if (end > begin) {
                    result.add(version.substring(begin, end));
                }
                begin = end + 1;
            }
        }
        return result.toArray(new String[0]);
    }

    /**
     * @param c1 character
     * @param c2 another character
     * @return will only return {@code true} if one of the characters is a digit and the other a letter
     */
    private static boolean isTokenBoundary(char c1, char c2) {
        return Character.isDigit(c1) ^ Character.isDigit(c2);
    }

    private static String integerItemOrZero(String item) {
        return StringUtils.isNumeric(item) ? item : "0";
    }

    /**
     * Returns the most major segment that can change.
     * All segments that are more major than this one are held in place.
     * @return segment that can change
     */
    public Segment getSegment() {
        return segment;
    }

    @Override
    public int compareTo(@Nullable ArtifactVersion other) {
        if (other == null) {
            return -1;
        }

        return comparable.compareTo(other);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        if (!(o instanceof BoundArtifactVersion)) {
            return false;
        }

        BoundArtifactVersion that = (BoundArtifactVersion) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o))
                .append(getSegment(), that.getSegment())
                .append(comparable, that.comparable)
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode())
                .append(getSegment())
                .append(comparable)
                .toHashCode();
    }

    @Override
    public int getMajorVersion() {
        return comparable.getMajorVersion();
    }

    @Override
    public int getMinorVersion() {
        return comparable.getMinorVersion();
    }

    @Override
    public int getIncrementalVersion() {
        return comparable.getIncrementalVersion();
    }

    @Override
    public int getBuildNumber() {
        return comparable.getBuildNumber();
    }

    @Override
    public String getQualifier() {
        return comparable.getQualifier();
    }

    /**
     * @deprecated do not use: this method would mutate the state and therefore is illegal to use
     * @throws UnsupportedOperationException thrown if the method is called
     */
    @Override
    @Deprecated
    public void parseVersion(String version) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return comparable.toString();
    }
}
