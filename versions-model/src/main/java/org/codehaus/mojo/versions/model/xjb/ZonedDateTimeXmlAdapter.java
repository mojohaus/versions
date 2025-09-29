package org.codehaus.mojo.versions.model.xjb;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class ZonedDateTimeXmlAdapter {
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");

    public static ZonedDateTime parse(String v) {
        return (v == null) ? null : ZonedDateTime.parse(v, FORMATTER);
    }

    public static String print(ZonedDateTime v) {
        return (v == null) ? null : v.format(FORMATTER);
    }
}
