package org.codehaus.mojo.versions.model.xjb;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Adapter for JAXB to handle {@link ZonedDateTime} serialization and deserialization.
 */
public class ZonedDateTimeXmlAdapter {
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");

    /**
     * Parses a string into a {@link ZonedDateTime}.
     * @param v the string to parse
     * @return the parsed ZonedDateTime
     */
    public static ZonedDateTime parse(String v) {
        return (v == null) ? null : ZonedDateTime.parse(v, FORMATTER);
    }

    /**
     * Formats a {@link ZonedDateTime} into a string.
     * @param v the ZonedDateTime to format
     * @return the formatted string
     */
    public static String print(ZonedDateTime v) {
        return (v == null) ? null : v.format(FORMATTER);
    }
}
