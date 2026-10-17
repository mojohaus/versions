package org.codehaus.mojo.versions.rewriting;

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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PropertyVersionInternalTest {

    private static final String PROPERTY_NAME = "myVersion";
    private static final String NEW_VERSION = "1.2.345";
    private static final String PROFILE_ID = "myProfile";
    private static final String INDENTATION = "  ";
    private static final String LINE_SEPARATOR = "\n";

    @Test
    void testInsertProjectPropertiesContainerDoesNotExist() throws Exception {
        testInsert(project(), project(projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertiesContainerDoesNotExist() throws Exception {
        testUpdateNoop(project());
    }

    @Test
    void testInsertProjectPropertiesContainerIsEmpty() throws Exception {
        testInsert(project(projectProperties()), project(projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertiesContainerIsEmpty() throws Exception {
        testUpdateNoop(project(projectProperties()));
    }

    @Test
    void testInsertProjectPropertiesContainerIsNotEmpty() throws Exception {
        testInsert(
                project(projectProperties("someKey", "someValue")),
                project(projectProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertiesContainerIsNotEmpty() throws Exception {
        testUpdateNoop(project(projectProperties("someKey", "someValue")));
    }

    @Test
    void testInsertProjectPropertyContainerMissingButProfileExists() throws Exception {
        testInsert(
                project(profiles(profile(profileId("ignoredProfile")))),
                project(profiles(profile(profileId("ignoredProfile"))), projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertyContainerMissingButProfileExists() throws Exception {
        testUpdateNoop(project(profiles(profile(profileId("ignoredProfile")))));
    }

    @Test
    void testInsertProjectPropertyContainerMissingButProfilePropertyExists() throws Exception {
        testInsert(
                project(profiles(
                        profile(profileId("ignoredProfile"), profileProperties(PROPERTY_NAME, "mustNotChange")))),
                project(
                        profiles(profile(
                                profileId("ignoredProfile"), profileProperties(PROPERTY_NAME, "mustNotChange"))),
                        projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertyContainerMissingButProfilePropertyExists() throws Exception {
        testUpdateNoop(project(
                profiles(profile(profileId("ignoredProfile"), profileProperties(PROPERTY_NAME, "mustNotChange")))));
    }

    @Test
    void testInsertProjectPropertyExists() throws Exception {
        testInsert(
                project(projectProperties(PROPERTY_NAME, "changeit")),
                project(projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertyExists() throws Exception {
        testUpdate(
                project(projectProperties(PROPERTY_NAME, "changeit")),
                project(projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testInsertProjectPropertyIsEmpty() throws Exception {
        testInsert(
                project(projectProperties("someKey", "someValue", PROPERTY_NAME, "")),
                project(projectProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertyIsEmpty() throws Exception {
        testUpdate(
                project(projectProperties("someKey", "someValue", PROPERTY_NAME, "")),
                project(projectProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testInsertProjectPropertyExistsTwice() throws Exception {
        testInsert(
                project(projectProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, "changeit")),
                project(projectProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertyExistsTwice() throws Exception {
        testUpdate(
                project(projectProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, "changeit")),
                project(projectProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testInsertProjectPropertyAlreadyUpToDate() throws Exception {
        testInsertNoop(project(projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testUpdateProjectPropertyAlreadyUpToDate() throws Exception {
        testUpdateNoop(project(projectProperties(PROPERTY_NAME, NEW_VERSION)));
    }

    @Test
    void testInsertProfileSectionDoesNotExist() throws Exception {
        testInsert(
                PROFILE_ID,
                project(),
                project(profiles(profile(profileId(PROFILE_ID), profileProperties(PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfileSectionDoesNotExist() throws Exception {
        testUpdateNoop(PROFILE_ID, project());
    }

    @Test
    void testInsertProfileSectionIsEmpty() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles()),
                project(profiles(profile(profileId(PROFILE_ID), profileProperties(PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfileSectionIsEmpty() throws Exception {
        testUpdateNoop(PROFILE_ID, project(profiles()));
    }

    @Test
    void testInsertProfileMissingButProjectPropertyExists() throws Exception {
        testInsert(
                PROFILE_ID,
                project(projectProperties(PROPERTY_NAME, "mustNotChange")),
                project(
                        projectProperties(PROPERTY_NAME, "mustNotChange"),
                        profiles(profile(profileId(PROFILE_ID), profileProperties(PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfileMissingButProjectPropertyExists() throws Exception {
        testUpdateNoop(PROFILE_ID, project(projectProperties(PROPERTY_NAME, "mustNotChange")));
    }

    @Test
    void testInsertProfileMissingButAnotherProfileExists() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileId("ignoredProfile")))),
                project(profiles(
                        profile(profileId("ignoredProfile")),
                        profile(profileId(PROFILE_ID), profileProperties(PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfileMissingButAnotherProfileExists() throws Exception {
        testUpdateNoop(PROFILE_ID, project(profiles(profile(profileId("ignoredProfile")))));
    }

    @Test
    void testInsertProfilePropertiesContainerDoesNotExist() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileId(PROFILE_ID)))),
                project(profiles(profile(profileId(PROFILE_ID), profileProperties(PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfilePropertiesContainerDoesNotExist() throws Exception {
        testUpdateNoop(PROFILE_ID, project(profiles(profile(profileId(PROFILE_ID)))));
    }

    @Test
    void testInsertProfilePropertiesContainerIsEmpty() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileId(PROFILE_ID), profileProperties()))),
                project(profiles(profile(profileId(PROFILE_ID), profileProperties(PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfilePropertiesContainerIsEmpty() throws Exception {
        testUpdateNoop(PROFILE_ID, project(profiles(profile(profileId(PROFILE_ID), profileProperties()))));
    }

    @Test
    void testInsertProfilePropertiesContainerExists() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileId(PROFILE_ID), profileProperties("someKey", "someValue")))),
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfilePropertiesContainerExists() throws Exception {
        testUpdateNoop(
                PROFILE_ID,
                project(profiles(profile(profileId(PROFILE_ID), profileProperties("someKey", "someValue")))));
    }

    @Test
    void testInsertProfilePropertiesContainerExistsBeforeId() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileProperties(), profileId(PROFILE_ID)))),
                project(profiles(profile(profileProperties(PROPERTY_NAME, NEW_VERSION), profileId(PROFILE_ID)))));
    }

    @Test
    void testUpdateProfilePropertiesContainerExistsBeforeId() throws Exception {
        testUpdateNoop(PROFILE_ID, project(profiles(profile(profileProperties(), profileId(PROFILE_ID)))));
    }

    @Test
    void testInsertProfilePropertyDoesNotExistBeforeId() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileProperties("someKey", "someValue"), profileId(PROFILE_ID)))),
                project(profiles(profile(
                        profileProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION),
                        profileId(PROFILE_ID)))));
    }

    @Test
    void testUpdateProfilePropertyDoesNotExistBeforeId() throws Exception {
        testUpdateNoop(
                PROFILE_ID,
                project(profiles(profile(profileProperties("someKey", "someValue"), profileId(PROFILE_ID)))));
    }

    @Test
    void testInsertProfilePropertyDoesNotExistBeforeIdFollowedByAnotherProfile() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(
                        profile(profileProperties("someKey", "someValue"), profileId(PROFILE_ID)),
                        profile(profileId("otherProfile"), profileProperties("otherKey", "otherValue")))),
                project(profiles(
                        profile(
                                profileProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION),
                                profileId(PROFILE_ID)),
                        profile(profileId("otherProfile"), profileProperties("otherKey", "otherValue")))));
    }

    @Test
    void testUpdateProfilePropertyDoesNotExistBeforeIdFollowedByAnotherProfile() throws Exception {
        testUpdateNoop(
                PROFILE_ID,
                project(profiles(
                        profile(profileProperties("someKey", "someValue"), profileId(PROFILE_ID)),
                        profile(profileId("otherProfile"), profileProperties("otherKey", "otherValue")))));
    }

    @Test
    void testInsertProfilePropertyExistsBeforeIdPrecededByAnotherProfile() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(
                        profile(profileProperties("otherKey", "otherValue"), profileId("otherProfile")),
                        profile(profileProperties("someKey", "someValue"), profileId(PROFILE_ID)))),
                project(profiles(
                        profile(profileProperties("otherKey", "otherValue"), profileId("otherProfile")),
                        profile(
                                profileProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION),
                                profileId(PROFILE_ID)))));
    }

    @Test
    void testUpdateProfilePropertyExistsBeforeIdPrecededByAnotherProfile() throws Exception {
        testUpdateNoop(
                PROFILE_ID,
                project(profiles(
                        profile(profileProperties("otherKey", "otherValue"), profileId("otherProfile")),
                        profile(profileProperties("someKey", "someValue"), profileId(PROFILE_ID)))));
    }

    @Test
    void testInsertProfilePropertyExists() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(
                        profileId(PROFILE_ID), profileProperties("someKey", "someValue", PROPERTY_NAME, "changeit")))),
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfilePropertyExists() throws Exception {
        testUpdate(
                PROFILE_ID,
                project(profiles(profile(
                        profileId(PROFILE_ID), profileProperties("someKey", "someValue", PROPERTY_NAME, "changeit")))),
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testInsertProfilePropertyExistsTwice() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, "changeit")))),
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testUpdateProfilePropertyExistsTwice() throws Exception {
        testUpdate(
                PROFILE_ID,
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, "changeit")))),
                project(profiles(profile(
                        profileId(PROFILE_ID),
                        profileProperties(PROPERTY_NAME, "mustNotChange", PROPERTY_NAME, NEW_VERSION)))));
    }

    @Test
    void testInsertProfilePropertyExistsBeforeId() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(profile(profileProperties(PROPERTY_NAME, "changeit"), profileId(PROFILE_ID)))),
                project(profiles(profile(profileProperties(PROPERTY_NAME, NEW_VERSION), profileId(PROFILE_ID)))));
    }

    @Test
    void testUpdateProfilePropertyExistsBeforeId() throws Exception {
        testUpdate(
                PROFILE_ID,
                project(profiles(profile(profileProperties(PROPERTY_NAME, "changeit"), profileId(PROFILE_ID)))),
                project(profiles(profile(profileProperties(PROPERTY_NAME, NEW_VERSION), profileId(PROFILE_ID)))));
    }

    @Test
    void testInsertProfilePropertyExistsBeforeIdFollowedByAnotherProfile() throws Exception {
        testInsert(
                PROFILE_ID,
                project(profiles(
                        profile(profileProperties(PROPERTY_NAME, "changeit"), profileId(PROFILE_ID)),
                        profile(profileProperties(PROPERTY_NAME, "mustNotChange"), profileId("otherProfile")))),
                project(profiles(
                        profile(profileProperties(PROPERTY_NAME, NEW_VERSION), profileId(PROFILE_ID)),
                        profile(profileProperties(PROPERTY_NAME, "mustNotChange"), profileId("otherProfile")))));
    }

    @Test
    void testUpdateProfilePropertyExistsBeforeIdFollowedByAnotherProfile() throws Exception {
        testUpdate(
                PROFILE_ID,
                project(profiles(
                        profile(profileProperties(PROPERTY_NAME, "changeit"), profileId(PROFILE_ID)),
                        profile(profileProperties(PROPERTY_NAME, "mustNotChange"), profileId("otherProfile")))),
                project(profiles(
                        profile(profileProperties(PROPERTY_NAME, NEW_VERSION), profileId(PROFILE_ID)),
                        profile(profileProperties(PROPERTY_NAME, "mustNotChange"), profileId("otherProfile")))));
    }

    @Test
    void testUpdateProjectPropertyExistsIgnoringProfiles() throws Exception {
        testUpdate(
                project(
                        projectProperties("someKey", "someValue", PROPERTY_NAME, "changeit"),
                        projectBuild(),
                        profiles(
                                profile(
                                        profileId("someOtherProfile"),
                                        profileProperties(PROPERTY_NAME, "mustNotChange")),
                                profile(
                                        profileId(PROFILE_ID),
                                        profileActivation(),
                                        profileProperties(
                                                "someKey", "someOtherValue", PROPERTY_NAME, "mustNotChange")))),
                project(
                        projectProperties("someKey", "someValue", PROPERTY_NAME, NEW_VERSION),
                        projectBuild(),
                        profiles(
                                profile(
                                        profileId("someOtherProfile"),
                                        profileProperties(PROPERTY_NAME, "mustNotChange")),
                                profile(
                                        profileId(PROFILE_ID),
                                        profileActivation(),
                                        profileProperties(
                                                "someKey", "someOtherValue", PROPERTY_NAME, "mustNotChange")))));
    }

    @Test
    void testUpdateUseProfileWhenProjectPropertyExists() throws Exception {
        testUpdate(
                PROFILE_ID,
                project(
                        projectProperties("someKey", "someValue", PROPERTY_NAME, "mustNotChange"),
                        projectBuild(),
                        profiles(
                                profile(
                                        profileId("someOtherProfile"),
                                        profileProperties(PROPERTY_NAME, "mustNotChange")),
                                profile(
                                        profileId(PROFILE_ID),
                                        profileActivation(),
                                        profileProperties("someKey", "someOtherValue", PROPERTY_NAME, "changeit")))),
                project(
                        projectProperties("someKey", "someValue", PROPERTY_NAME, "mustNotChange"),
                        projectBuild(),
                        profiles(
                                profile(
                                        profileId("someOtherProfile"),
                                        profileProperties(PROPERTY_NAME, "mustNotChange")),
                                profile(
                                        profileId(PROFILE_ID),
                                        profileActivation(),
                                        profileProperties("someKey", "someOtherValue", PROPERTY_NAME, NEW_VERSION)))));
    }

    /*
     * Test execution helpers
     */

    static void testInsert(String profileId, String input, String output) throws Exception {
        test(true, profileId, input, output);
    }

    static void testInsert(String input, String output) throws Exception {
        testInsert(null, input, output);
    }

    static void testInsertNoop(String profileId, String input) throws Exception {
        test(true, profileId, input, input);
    }

    static void testInsertNoop(String input) throws Exception {
        testInsertNoop(null, input);
    }

    static void testUpdate(String profileId, String input, String output) throws Exception {
        test(false, profileId, input, output);
    }

    static void testUpdate(String input, String output) throws Exception {
        testUpdate(null, input, output);
    }

    static void testUpdateNoop(String profileId, String input) throws Exception {
        test(false, profileId, input, input);
    }

    static void testUpdateNoop(String input) throws Exception {
        testUpdateNoop(null, input);
    }

    static void test(boolean insert, String profileId, String input, String output) throws Exception {
        MutableXMLStreamReader pom = new MutableXMLStreamReader(
                new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)), new File("pom.xml").toPath());

        boolean modified = new PropertyVersionInternal(pom, profileId, PROPERTY_NAME, NEW_VERSION).update(insert);

        String result = pom.getSource();
        assertEquals(output, result);
        assertEquals(!input.equals(result), modified, "Modification status does not match actual change in content");
    }

    /* Some test data generators */

    /**
     * Some default project content to have some indentation info.
     */
    static String defaultProjectContent() {
        return INDENTATION + "<modelVersion>4.0.0</modelVersion>" + LINE_SEPARATOR;
    }

    static String project(String... projectContent) {
        return "<project>" + LINE_SEPARATOR
                + defaultProjectContent()
                + (projectContent.length == 0 ? "" : String.join("", projectContent))
                + "</project>" + LINE_SEPARATOR;
    }

    /**
     * Just to adds some noise.
     */
    static String projectBuild() {
        return INDENTATION + "<build>" + LINE_SEPARATOR + "</build>" + LINE_SEPARATOR + LINE_SEPARATOR;
    }

    static String profileActivation() {
        return INDENTATION + INDENTATION + INDENTATION + "<activation>" + LINE_SEPARATOR
                + "        <activeByDefault>false</activeByDefault>" + LINE_SEPARATOR
                + INDENTATION + INDENTATION + INDENTATION + "</activation>" + LINE_SEPARATOR + LINE_SEPARATOR;
    }

    static String profileProperties(Map<String, String> properties) {
        return INDENTATION + INDENTATION + INDENTATION + "<properties>" + LINE_SEPARATOR
                + properties.entrySet().stream()
                        .map(e -> INDENTATION + INDENTATION + INDENTATION + INDENTATION + "<" + e.getKey() + ">"
                                + e.getValue() + "</" + e.getKey() + ">" + LINE_SEPARATOR)
                        .reduce("", String::concat)
                + INDENTATION + INDENTATION + INDENTATION + "</properties>" + LINE_SEPARATOR;
    }

    static String profiles(String... profilesContent) {
        return INDENTATION + "<profiles>" + LINE_SEPARATOR
                + (profilesContent.length == 0 ? "" : String.join("", profilesContent))
                + INDENTATION + "</profiles>" + LINE_SEPARATOR;
    }

    static String profile(String... profileContent) {
        return INDENTATION + INDENTATION + "<profile>" + LINE_SEPARATOR
                + (profileContent.length == 0 ? "" : String.join("", profileContent))
                + INDENTATION + INDENTATION + "</profile>" + LINE_SEPARATOR;
    }

    static String profileId(String profileId) {
        return INDENTATION + INDENTATION + INDENTATION + "<id>" + profileId + "</id>" + LINE_SEPARATOR;
    }

    static String profileIdRaw(String profileIdRaw) {
        return INDENTATION + INDENTATION + INDENTATION + "<id>" + profileIdRaw + "</id>" + LINE_SEPARATOR;
    }

    static String profileProperties() {
        Map<String, String> properties = new LinkedHashMap<>();
        return profileProperties(properties);
    }

    static String profileProperties(String key, String value) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put(key, value);
        return profileProperties(properties);
    }

    static String profileProperties(String key1, String value1, String key2, String value2) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put(key1, value1);
        properties.put(key2, value2);
        return profileProperties(properties);
    }

    static String projectProperties(Map<String, String> properties) {
        return INDENTATION + "<properties>" + LINE_SEPARATOR
                + properties.entrySet().stream()
                        .map(e -> INDENTATION + INDENTATION + "<" + e.getKey() + ">" + e.getValue() + "</" + e.getKey()
                                + ">" + LINE_SEPARATOR)
                        .reduce("", String::concat)
                + INDENTATION + "</properties>" + LINE_SEPARATOR;
    }

    static String projectProperties() {
        Map<String, String> properties = new LinkedHashMap<>();
        return projectProperties(properties);
    }

    static String projectProperties(String key, String value) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put(key, value);
        return projectProperties(properties);
    }

    static String projectProperties(String key1, String value1, String key2, String value2) {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put(key1, value1);
        properties.put(key2, value2);
        return projectProperties(properties);
    }
}
