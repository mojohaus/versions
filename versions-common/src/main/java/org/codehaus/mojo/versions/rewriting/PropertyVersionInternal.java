package org.codehaus.mojo.versions.rewriting;

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
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.xml.stream.XMLStreamException;

/**
 * Searches the pom re-defining or inserting the specified version property.
 * <p>
 * This is an internal implementation class used by {@link org.codehaus.mojo.versions.api.PomHelper}.
 * It should not be used directly by client code.
 *
 * @since 2.20.1
 */
public class PropertyVersionInternal {

    private final MutableXMLStreamReader pom;
    private final String profileId;
    private final String propertyName;
    private final String value;
    private final String indentation;
    private final String lineSeparator;

    private Template result = Template.NONE;
    private boolean inProfileWithId;

    /**
     * Marks.
     */
    private enum Marks {
        START,
        END
    }

    /**
     * Represents the state of the parser.
     */
    private enum State {
        BEGIN_OF_STREAM,
        PROJECT,
        PROFILES,
        PROFILE,
        PROFILE_ID,
        PROPERTIES,
        PROPERTY,
        PROJECT_END;
    }

    /**
     * Result template.
     *
     * Decides what to render between start and end marks.
     */
    private enum Template {
        /**
         * Does not update anything.
         */
        NONE {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                return false;
            }
        },
        /**
         * Replaces an existing property value.
         */
        VALUE {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(Marks.START, Marks.END, value);
                return true;
            }
        },
        /**
         * Inserts a missing property entry at the end of the {@code properties} container.
         */
        PROJECT_PROPERTY {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(
                        Marks.START,
                        Marks.END,
                        String.format(
                                "%3$s<%1$s>%2$s</%1$s>%4$s%3$s", propertyName, value, indentation, lineSeparator));
                return true;
            }
        },
        /**
         * Inserts a missing property entry at the end of the {@code profile}'s {@code properties} container.
         */
        PROFILE_PROPERTY {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(
                        Marks.START,
                        Marks.END,
                        String.format(
                                "%3$s<%1$s>%2$s</%1$s>%4$s%3$s%3$s%3$s",
                                propertyName, value, indentation, lineSeparator));
                return true;
            }
        },
        /**
         * Inserts a missing property container at the end of the {@code project} element.
         */
        PROJECT_PROPERTIES {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(
                        Marks.START,
                        Marks.END,
                        String.format(
                                "%3$s<properties>%4$s%3$s%3$s<%1$s>%2$s</%1$s>%4$s%3$s</properties>%4$s",
                                propertyName, value, indentation, lineSeparator));
                return true;
            }
        },
        /**
         * Inserts a missing property container at the end of the {@code profile} element.
         */
        PROFILE_PROPERTIES {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(
                        Marks.START,
                        Marks.END,
                        String.format(
                                "%3$s<properties>%4$s%3$s%3$s%3$s%3$s<%1$s>%2$s</%1$s>%4$s%3$s%3$s%3$s</properties>%4$s%3$s%3$s",
                                propertyName, value, indentation, lineSeparator));
                return true;
            }
        };

        /**
         * Applies the template.
         *
         * @return {@code true} if the pom was modified
         */
        abstract boolean apply(
                MutableXMLStreamReader pom,
                String propertyName,
                String value,
                String indentation,
                String lineSeparator);
    }

    /**
     * Constructor.
     *
     * @param pom          The pom to modify.
     * @param profileId    The profile in which to modify the property.
     * @param propertyName The property to modify.
     * @param value        The new value of the property.
     */
    public PropertyVersionInternal(MutableXMLStreamReader pom, String profileId, String propertyName, String value) {
        this.pom = pom;
        this.profileId = profileId;
        this.propertyName = propertyName;
        this.value = value;
        this.indentation = pom.getIndentation();
        this.lineSeparator = pom.getLineSeparator();
    }

    /**
     * Perform the actual update operation.
     *
     * @param insert Whether to insert a new property or just replace an existing one.
     * @return {@code true} if the pom was modified
     * @throws XMLStreamException if something went wrong.
     */
    public boolean update(boolean insert) throws XMLStreamException {
        pom.rewind();
        result = Template.NONE;

        State state = State.BEGIN_OF_STREAM;
        while (result == Template.NONE && state != State.PROJECT_END && pom.hasNext()) {
            pom.next();

            if (pom.isStartElement()) {
                state = handleStartElement(state, pom.getLocalName());
            } else if (pom.isEndElement()) {
                state = handleEndElement(state, pom.getLocalName());
            } else {
                if (state != State.PROPERTY) {
                    pom.mark(Marks.START);
                }
            }
        }

        boolean replaced = false;
        if (insert || result == Template.VALUE) {
            replaced = result.apply(pom, propertyName, value, indentation, lineSeparator);
        }

        pom.clearMark(Marks.START);
        pom.clearMark(Marks.END);
        return replaced;
    }

    private State handleStartElement(State state, String name) throws XMLStreamException {
        switch (state) {
            case BEGIN_OF_STREAM:
                if ("project".equals(name)) {
                    pom.mark(Marks.START);
                    return State.PROJECT;
                }
                break;
            case PROJECT:
                if (profileId == null) {
                    if ("properties".equals(name)) {
                        pom.mark(Marks.START);
                        return State.PROPERTIES;
                    }
                } else {
                    if ("profiles".equals(name)) {
                        return State.PROFILES;
                    }
                }
                break;
            case PROFILES:
                if ("profile".equals(name)) {
                    return State.PROFILE;
                }
                break;
            case PROFILE:
                if ("id".equals(name)) {
                    return handleStartProfileId();
                }
                if ("properties".equals(name) && inProfileWithId) {
                    pom.mark(Marks.START);
                    return State.PROPERTIES;
                }
                break;
            case PROPERTIES:
                if (propertyName.equals(name)) {
                    pom.mark(Marks.START);
                    return State.PROPERTY;
                }
                break;
            default:
                break;
        }

        pom.skipElement();
        pom.mark(Marks.START);
        return state;
    }

    private State handleStartProfileId() throws XMLStreamException {
        if (profileId.equals(pom.getElementText().trim())) {
            inProfileWithId = true;
        }
        // getElementText could've pushed the pointer to END_ELEMENT
        if (!pom.isEndElement()) {
            return State.PROFILE_ID;
        }
        pom.mark(Marks.START);
        return State.PROFILE;
    }

    private State handleEndElement(State state, String name) {
        switch (state) {
            case PROJECT:
                if ("project".equals(name)) {
                    return handleEndProject();
                }
                break;
            case PROFILES:
                if ("profiles".equals(name)) {
                    return State.PROJECT;
                }
                break;
            case PROFILE:
                if ("profile".equals(name)) {
                    return handleEndProfile();
                }
                break;
            case PROFILE_ID:
                if ("id".equals(name)) {
                    return State.PROFILE;
                }
                break;
            case PROPERTIES:
                if ("properties".equals(name)) {
                    return handleEndProperties();
                }
                break;
            case PROPERTY:
                if (propertyName.equals(name)) {
                    return handleEndProperty();
                }
                break;
            default:
                break;
        }
        return state;
    }

    private State handleEndProject() {
        if (result == Template.NONE && profileId == null) {
            pom.mark(Marks.END);
            result = Template.PROJECT_PROPERTIES;
        }
        return State.PROJECT_END;
    }

    private State handleEndProfile() {
        if (result == Template.NONE && inProfileWithId) {
            pom.mark(Marks.END);
            result = Template.PROFILE_PROPERTIES;
        }

        inProfileWithId = false;
        return State.PROFILES;
    }

    private State handleEndProperties() {
        if (profileId == null) {
            if (result == Template.NONE) {
                pom.mark(Marks.END);
                result = Template.PROJECT_PROPERTY;
            }
            return State.PROJECT;
        }
        if (result == Template.NONE) {
            pom.mark(Marks.END);
            result = Template.PROFILE_PROPERTY;
        }
        return State.PROFILE;
    }

    private State handleEndProperty() {
        pom.mark(Marks.END);
        result = Template.VALUE;
        return State.PROPERTIES;
    }
}
