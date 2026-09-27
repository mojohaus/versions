/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import groovy.xml.XmlSlurper

def model = { path -> new XmlSlurper().parse(new File(basedir, path + '/pom.xml')) }
assert model('parent').version.text() == '2.0'
assert model('parent/component').parent.version.text() == '2.0'
assert model('tests').parent.version.text() == '2.0'
assert model('tests/child').parent.version.text() == '2.0'
assert model('bom').version.text() == '2.0'
assert model('bom').dependencyManagement.dependencies.dependency.version.text() == '2.0'
assert model('tests').dependencyManagement.dependencies.dependency.version.text() == '2.0'
assert model('unrelated').parent.version.text() == '9.0'
assert new XmlSlurper().parse(new File(basedir, 'pom.xml')).version.text() == '9.0'
