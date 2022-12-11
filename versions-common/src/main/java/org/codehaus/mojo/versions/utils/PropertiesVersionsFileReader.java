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
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import org.codehaus.mojo.versions.api.Property;

/**
 * Reader class for reading property files
 */
public class PropertiesVersionsFileReader {

    /**
     * Commas-separated list of properties
     */
    private String propertiesCsv;

    private Property[] propertiesConfig;

    private String propertyFilePath;

    /**
     * Creates an instance of the object with the given path to the property file
     * @param filePath path to the property file
     */
    public PropertiesVersionsFileReader(String filePath) {
        propertyFilePath = filePath;
    }

    /**
     * Reads the property file
     * @throws IOException thrown if an I/O exception occurs during the read operation
     */
    public void read() throws IOException {
        try (InputStream input = Files.newInputStream(Paths.get(propertyFilePath))) {

            Properties prop = new Properties();

            // load a properties file
            prop.load(input);

            prop.propertyNames();

            propertiesCsv = prop.keySet().stream().map(Object::toString).collect(Collectors.joining(","));

            List<Property> propertiesConfigList = new ArrayList<>();
            prop.forEach((name, version) -> {
                Property propertyConfig = new Property((String) name);
                propertyConfig.setVersion((String) version);
                propertiesConfigList.add(propertyConfig);
            });

            propertiesConfig = propertiesConfigList.toArray(new Property[0]);
        }
    }

    /**
     * Returns the string contents of the property file
     * @return contents of the property file
     */
    public String getProperties() {
        return propertiesCsv;
    }

    /**
     * Returns the array of {@link Property} objects
     * @return array of properties
     */
    public Property[] getPropertiesConfig() {
        return propertiesConfig;
    }
}
