package org.codehaus.mojo.versions.utils;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class PropertiesVersionsFileReaderTest {

    private static final String TEST_PROPERTIES_FILE = "src/test/resources/org/codehaus/mojo/versions/utils/testPropertiesVersionsFile.properties";

    @Test
    public void testRead() throws IOException {
        PropertiesVersionsFileReader reader = new PropertiesVersionsFileReader(TEST_PROPERTIES_FILE);
        reader.read();

        int numberOfPropertiesConfig=3;
        assertTrue(equalsCvsUnordered("booking-api.version,booking-lib.version,be-air-impl.version", reader.getProperties()));
        assertEquals(numberOfPropertiesConfig, reader.getPropertiesConfig().length);
    }

    private boolean equalsCvsUnordered(String csvExpected, String csvActual) {
        if (StringUtils.isEmpty(csvExpected)) {
            return false;
        }
        Set<String> listExpected = new HashSet<String>(Arrays.asList(csvExpected.split(",")));
        Set<String> listActual = new HashSet<String>(Arrays.asList(csvActual.split(",")));
        return listExpected.equals(listActual);
    }
}
