package org.codehaus.mojo.versions;

/*
 * The MIT License
 *
 * Copyright (c) 2016, 2017 Karl Heinz Marbaise
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class will parse the version based on the given pattern in
 * {@code  org.codehaus.mojo.buildhelper.ParseVersionMojo}.
 *
 * @author Karl Heinz Marbaise
 * <a href="mailto:khmarbaise@apache.org">khmarbaise@apache.org</a>
 *
 */
public class VersionInformation {

    private static final String MAJOR_MINOR_PATCH_PATTERN = "^((\\d+)(\\.(\\d+)(\\.(\\d+))?)?)";

    private static final Pattern MAJOR_MINOR_PATCH = Pattern.compile(MAJOR_MINOR_PATCH_PATTERN);

    private static final Pattern DIGITS = Pattern.compile(MAJOR_MINOR_PATCH_PATTERN + "(.*)$");

    private static final Pattern BUILD_NUMBER = Pattern.compile("(((\\-)(\\d+)(.*))?)|(\\.(.*))|(\\-(.*))|(.*)$");

    private int major;

    private int minor;

    private int patch;

    private long buildNumber;

    private String qualifier;

    private void parseBuildNumber(String buildNumberPart) {
        Matcher matcher = BUILD_NUMBER.matcher(buildNumberPart);
        if (matcher.matches()) {
            String buildNumber = matcher.group(4);
            String qualifier = matcher.group(5);

            if (buildNumber != null) {
                setBuildNumber(Long.parseLong(buildNumber));
            }

            if (matcher.group(7) != null) {
                qualifier = matcher.group(7);
            }
            // Starting with "-"
            if (matcher.group(9) != null) {
                qualifier = matcher.group(9);
            }
            if (qualifier != null) {
                if (qualifier.trim().length() == 0) {
                    setQualifier(null);
                } else {
                    setQualifier(qualifier);
                }
            } else {
                setQualifier(null);
            }
        }
    }

    private void parseMajorMinorPatchVersion(String version) {
        Matcher matcher = MAJOR_MINOR_PATCH.matcher(version);
        if (matcher.matches()) {
            String majorString = matcher.group(2);
            String minorString = matcher.group(4);
            String patchString = matcher.group(6);

            if (majorString != null) {
                setMajor(Integer.parseInt(majorString));
            }
            if (minorString != null) {
                setMinor(Integer.parseInt(minorString));
            }
            if (patchString != null) {
                setPatch(Integer.parseInt(patchString));
            }
        }
    }

    public VersionInformation(String version) {
        Matcher matcherDigits = DIGITS.matcher(version);
        if (matcherDigits.matches()) {
            parseMajorMinorPatchVersion(matcherDigits.group(1));
            parseBuildNumber(matcherDigits.group(7));
        } else {
            setQualifier(version);
        }
    }

    public int getMajor() {
        return major;
    }

    public void setMajor(int major) {
        this.major = major;
    }

    public int getMinor() {
        return minor;
    }

    public void setMinor(int minor) {
        this.minor = minor;
    }

    public int getPatch() {
        return patch;
    }

    public void setPatch(int patch) {
        this.patch = patch;
    }

    public long getBuildNumber() {
        return buildNumber;
    }

    public void setBuildNumber(long buildNumber) {
        this.buildNumber = buildNumber;
    }

    public String getQualifier() {
        return qualifier;
    }

    public void setQualifier(String qualifier) {
        this.qualifier = qualifier;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getMajor());
        sb.append("." + this.getMinor());
        sb.append("." + this.getPatch());

        if (this.getQualifier() != null || this.getBuildNumber() != 0) {

            if (this.getBuildNumber() != 0) {
                sb.append("-");
                sb.append(this.getBuildNumber());
            }
            if (this.getQualifier() != null) {
                sb.append("-");
                sb.append(this.getQualifier());
            }
        }

        return sb.toString();
    }
}
