package org.codehaus.mojo.versions.utils;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PropertiesVersionsFileReaderTest {

    private static final String TEST_PROPERTIES_FILE =
            "src/test/resources/org/codehaus/mojo/versions/utils/testPropertiesVersionsFile.properties";

    @Test
    void testRead() throws IOException {
        PropertiesVersionsFileReader reader = new PropertiesVersionsFileReader(TEST_PROPERTIES_FILE);
        reader.read();

        int numberOfPropertiesConfig = 3;
        assertTrue(equalsCvsUnordered(
                "booking-api.version,booking-lib.version,be-air-impl.version", reader.getProperties()));
        assertEquals(numberOfPropertiesConfig, reader.getPropertiesConfig().length);
    }

    private boolean equalsCvsUnordered(String csvExpected, String csvActual) {
        if (StringUtils.isEmpty(csvExpected)) {
            return false;
        }
        Set<String> listExpected = new HashSet<>(Arrays.asList(csvExpected.split(",")));
        Set<String> listActual = new HashSet<>(Arrays.asList(csvActual.split(",")));
        return listExpected.equals(listActual);
    }
}
