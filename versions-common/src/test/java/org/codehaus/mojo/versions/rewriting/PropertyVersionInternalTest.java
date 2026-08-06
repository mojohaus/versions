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
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PropertyVersionInternalTest {

    @ParameterizedTest
    @MethodSource("updatablePoms")
    void testUpdate(String xml, String profileId) throws Exception {
        MutableXMLStreamReader pom = new MutableXMLStreamReader(
                new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8)), new File("pom.xml").toPath());

        String oldVersion = "changeit";
        String newVersion = "1.2.345";
        boolean modified = new PropertyVersionInternal(pom, profileId, "myVersion", newVersion).update(true);
        assertTrue(modified);

        String result = pom.getSource();

        assertEquals(1, StringUtils.countMatches(result, "<myVersion>" + newVersion + "</myVersion>"));
        assertEquals(0, StringUtils.countMatches(result, oldVersion));
    }

    static Stream<Arguments> updatablePoms() {
        return Stream.of(
                Arguments.of(simplePom(), null),
                Arguments.of(simplePomWithPropertiesEmpty(), null),
                Arguments.of(simplePomWithProperties(), null),
                Arguments.of(simplePomWithPropertyExists(), null),
                Arguments.of(simplePomWithPropertyNotEmpty(), null),
                Arguments.of(simplePomWithProfile(), null),
                Arguments.of(simplePomWithProfile(), "myProfile"),
                Arguments.of(simplePomWithProfileAndPropertiesEmpty(), "myProfile"),
                Arguments.of(simplePomWithProfileAndProperties(), "myProfile"),
                Arguments.of(simplePomWithProfileAndPropertyExists(), "myProfile"),
                Arguments.of(simplePomWithProfileAndPropertyNotEmpty(), "myProfile"),
                Arguments.of(complexPomWithProfiles(), "myProfile"));
    }

    static String simplePom() {
        return "<project></project>";
    }

    static String simplePomWithPropertiesEmpty() {
        return "<project>\n" + "  <properties>\n" + "  </properties>\n" + "</project>\n";
    }

    static String simplePomWithProperties() {
        return "<project>\n"
                + "  <properties>\n"
                + "    <someKey>someValue</someKey>\n"
                + "  </properties>\n"
                + "</project>\n";
    }

    static String simplePomWithPropertyExists() {
        return "<project>\n"
                + "  <properties>\n"
                + "    <someKey>someValue</someKey>\n"
                + "    <myVersion></myVersion>\n"
                + "  </properties>\n"
                + "</project>\n";
    }

    static String simplePomWithPropertyNotEmpty() {
        return "<project>\n"
                + "  <properties>\n"
                + "    <someKey>someValue</someKey>\n"
                + "    <myVersion>changeit</myVersion>\n"
                + "  </properties>\n"
                + "</project>\n";
    }

    static String simplePomWithProfile() {
        return "<project>\n"
                + "  <profiles>\n"
                + "    <profile>\n"
                + "      <id>myProfile</id>\n"
                + "    </profile>\n"
                + "  </profiles>\n"
                + "</project>\n";
    }

    static String simplePomWithProfileAndPropertiesEmpty() {
        return "<project>\n"
                + "  <profiles>\n"
                + "    <profile>\n"
                + "      <id>myProfile</id>\n"
                + "      <properties>\n"
                + "      </properties>\n"
                + "    </profile>\n"
                + "  </profiles>\n"
                + "</project>\n";
    }

    static String simplePomWithProfileAndProperties() {
        return "<project>\n"
                + "  <profiles>\n"
                + "    <profile>\n"
                + "      <id>myProfile</id>\n"
                + "      <properties>\n"
                + "        <someKey>someValue</someKey>\n"
                + "      </properties>\n"
                + "    </profile>\n"
                + "  </profiles>\n"
                + "</project>\n";
    }

    static String simplePomWithProfileAndPropertyExists() {
        return "<project>\n"
                + "  <profiles>\n"
                + "    <profile>\n"
                + "      <id>myProfile</id>\n"
                + "      <properties>\n"
                + "        <someKey>someValue</someKey>\n"
                + "        <myVersion></myVersion>\n"
                + "      </properties>\n"
                + "    </profile>\n"
                + "  </profiles>\n"
                + "</project>\n";
    }

    static String simplePomWithProfileAndPropertyNotEmpty() {
        return "<project>\n"
                + "  <profiles>\n"
                + "    <profile>\n"
                + "      <id>myProfile</id>\n"
                + "      <properties>\n"
                + "        <someKey>someValue</someKey>\n"
                + "        <myVersion>changeit</myVersion>\n"
                + "      </properties>\n"
                + "    </profile>\n"
                + "  </profiles>\n"
                + "</project>\n";
    }

    static String complexPomWithProfiles() {
        return "<project>\n"
                + "  <!-- Ignore project properties -->\n"
                + "  <properties>\n"
                + "    <someKey>someValue</someKey>\n"
                + "    <myVersion>mustNotChange</myVersion>\n"
                + "  </properties>\n\n"
                + "  <!-- Ignore unrelated element -->\n"
                + "  <build>"
                + "  </build>\n\n"
                + "  <profiles>\n"
                + "    <!-- Ignore this profile -->\n"
                + "    <profile>\n"
                + "      <id>someOtherProfile</id>\n"
                + "      <properties>\n"
                + "        <myVersion>mustNotChange</myVersion>\n"
                + "      </properties>\n"
                + "    </profile>\n\n"
                + "    <profile>\n"
                + "      <id>myProfile</id>\n"
                + "      <!-- Ignore unrelated element -->\n"
                + "      <activation>\n"
                + "        <activeByDefault>false</activeByDefault>\n"
                + "      </activation>\n"
                + "      <properties>\n"
                + "        <someKey>someOtherValue</someKey>\n"
                + "        <myVersion>changeit</myVersion>\n"
                + "      </properties>\n"
                + "    </profile>\n"
                + "  </profiles>\n"
                + "</project>\n";
    }
}
