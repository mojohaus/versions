package org.codehaus.mojo.versions.utils;

import org.codehaus.mojo.versions.Property;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

public class PropertiesVersionsFileReader {

    /**
     * Commas-separated list of properties
     */
    private String propertiesCsv;

    private Property[] propertiesConfig;

    private String propertyFilePath;

    public PropertiesVersionsFileReader(String filePath) {
        propertyFilePath = filePath;
    }

    public void read() throws IOException {
        try (InputStream input = new FileInputStream(propertyFilePath)) {

            Properties prop = new Properties();

            // load a properties file
            prop.load(input);

            prop.propertyNames();

            propertiesCsv = prop.keySet().stream().map(Object::toString).collect(Collectors.joining(","));

            List<Property> propertiesConfigList = new ArrayList<>();
            prop.forEach((name, version) ->  {
                Property propertyConfig = new Property((String)name);
                propertyConfig.setVersion((String)version);
                propertiesConfigList.add(propertyConfig);
            });

            propertiesConfig = propertiesConfigList.toArray(new Property[0]);
        }
    }

    public String getProperties() {
        return propertiesCsv;
    }

    public Property[] getPropertiesConfig() {
        return propertiesConfig;
    }
}
