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
 * This is an internal implemenation class used by {@link org.codehaus.mojo.versions.api.PomHelper}.
 * It should not be used directly by client code.
 *
 * @since 2.23.1
 */
public class PropertyVersionInternal {

    private final MutableXMLStreamReader pom;
    private final String profileId;
    private final String propertyName;
    private final String value;
    private final String indentation;
    private final String lineSeparator;

    private Template result = Template.NONE;

    /**
     * Marks.
     */
    private enum Marks {
        START,
        END,
        VALUE_START,
        VALUE_END,
        PRE_PROFILE_PROPERTY_START,
        PRE_PROFILE_PROPERTY_END,
        PRE_PROFILE_VALUE_START,
        PRE_PROFILE_VALUE_END
    }

    /**
     * Represents the state of the parser.
     */
    private enum State {
        BEGIN_OF_STREAM,
        PROJECT,
        PROJECT_PROPERTIES,
        PROJECT_PROPERTY,

        PROFILES,

        PROFILE_CANDIDATE,
        PROFILE_CANDIDATE_WITH_PRE_ID_PROPERTIES,
        PROFILE_CANDIDATE_WITH_PRE_ID_VALUE,

        PROFILE_MATCHED,
        PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES,
        PROFILE_MATCHED_WITH_PRE_ID_VALUE,
        PROFILE_MATCHED_PROPERTIES,
        PROFILE_MATCHED_PROPERTY,
        PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTIES,
        PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTY,
        PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTIES,
        PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTY,
        PROFILE_UNMATCHED,

        PROJECT_END;

        boolean isCandidate() {
            return this == PROFILE_CANDIDATE
                    || this == PROFILE_CANDIDATE_WITH_PRE_ID_PROPERTIES
                    || this == PROFILE_CANDIDATE_WITH_PRE_ID_VALUE;
        }

        boolean isPropertyValueState() {
            return this == State.PROJECT_PROPERTY
                    || this == State.PROFILE_MATCHED_PROPERTY
                    || this == State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTY
                    || this == State.PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTY;
        }
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
                pom.replaceBetween(Marks.VALUE_START, Marks.VALUE_END, value);
                return true;
            }
        },
        /**
         * Replaces a property value found in a profile section scanned before its {@code id} was encountered.
         */
        PRE_PROFILE_VALUE {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(Marks.PRE_PROFILE_VALUE_START, Marks.PRE_PROFILE_VALUE_END, value);
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
         * Inserts a missing property entry into a profile {@code properties} section that appears before profile {@code id}.
         */
        PRE_PROFILE_PROPERTY {
            @Override
            boolean apply(
                    MutableXMLStreamReader pom,
                    String propertyName,
                    String value,
                    String indentation,
                    String lineSeparator) {
                pom.replaceBetween(
                        Marks.PRE_PROFILE_PROPERTY_START,
                        Marks.PRE_PROFILE_PROPERTY_END,
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
        // Keep scanning until project end so duplicate properties resolve to the last declaration (Maven behavior).
        while (state != State.PROJECT_END && pom.hasNext()) {
            pom.next();

            if (pom.isStartElement()) {
                state = handleStartElement(state, pom.getLocalName());
            } else if (pom.isEndElement()) {
                state = handleEndElement(state, pom.getLocalName());
            } else {
                if (!state.isPropertyValueState()) {
                    markStartWhileSearchingInsertionTarget();
                }
            }
        }

        boolean replaced = false;
        if (insert || result == Template.VALUE || result == Template.PRE_PROFILE_VALUE) {
            replaced = result.apply(pom, propertyName, value, indentation, lineSeparator);
        }

        pom.clearMark(Marks.START);
        pom.clearMark(Marks.END);
        pom.clearMark(Marks.VALUE_START);
        pom.clearMark(Marks.VALUE_END);
        pom.clearMark(Marks.PRE_PROFILE_PROPERTY_START);
        pom.clearMark(Marks.PRE_PROFILE_PROPERTY_END);
        pom.clearMark(Marks.PRE_PROFILE_VALUE_START);
        pom.clearMark(Marks.PRE_PROFILE_VALUE_END);
        return replaced;
    }

    private State handleStartElement(State state, String name) throws XMLStreamException {
        switch (state) {
            case BEGIN_OF_STREAM:
                if ("project".equals(name)) {
                    markStartWhileSearchingInsertionTarget();
                    return State.PROJECT;
                }
                break;
            case PROJECT:
                if (profileId == null) {
                    if ("properties".equals(name)) {
                        markStartWhileSearchingInsertionTarget();
                        return State.PROJECT_PROPERTIES;
                    }
                } else {
                    if ("profiles".equals(name)) {
                        return State.PROFILES;
                    }
                }
                break;
            case PROFILES:
                if ("profile".equals(name)) {
                    clearPreIdProfileMarks();
                    return State.PROFILE_CANDIDATE;
                }
                break;
            case PROFILE_CANDIDATE:
            case PROFILE_CANDIDATE_WITH_PRE_ID_PROPERTIES:
            case PROFILE_CANDIDATE_WITH_PRE_ID_VALUE:
            case PROFILE_MATCHED:
            case PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES:
            case PROFILE_MATCHED_WITH_PRE_ID_VALUE:
            case PROFILE_UNMATCHED:
                if ("id".equals(name)) {
                    return handleStartProfileId(state);
                }
                if ("properties".equals(name)) {
                    if (state == State.PROFILE_MATCHED) {
                        markStartWhileSearchingInsertionTarget();
                        return State.PROFILE_MATCHED_PROPERTIES;
                    }
                    if (state == State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES) {
                        markStartWhileSearchingInsertionTarget();
                        return State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTIES;
                    }
                    if (state == State.PROFILE_MATCHED_WITH_PRE_ID_VALUE) {
                        markStartWhileSearchingInsertionTarget();
                        return State.PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTIES;
                    }
                    if (state.isCandidate()) {
                        return scanPropertiesBeforeProfileId(state);
                    } else {
                        pom.skipElement();
                    }
                    return state;
                }
                break;
            case PROJECT_PROPERTIES:
                if (propertyName.equals(name)) {
                    pom.mark(Marks.VALUE_START);
                    return State.PROJECT_PROPERTY;
                }
                break;
            case PROFILE_MATCHED_PROPERTIES:
                if (propertyName.equals(name)) {
                    pom.mark(Marks.VALUE_START);
                    return State.PROFILE_MATCHED_PROPERTY;
                }
                break;
            case PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTIES:
                if (propertyName.equals(name)) {
                    pom.mark(Marks.VALUE_START);
                    return State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTY;
                }
                break;
            case PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTIES:
                if (propertyName.equals(name)) {
                    pom.mark(Marks.VALUE_START);
                    return State.PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTY;
                }
                break;
            default:
                break;
        }

        pom.skipElement();
        markStartWhileSearchingInsertionTarget();
        return state;
    }

    private void markStartWhileSearchingInsertionTarget() {
        if (result == Template.NONE) {
            pom.mark(Marks.START);
        }
    }

    private State handleStartProfileId(State state) throws XMLStreamException {
        if (profileId.equals(pom.getElementText().trim())) {
            if (state == State.PROFILE_CANDIDATE_WITH_PRE_ID_VALUE) {
                result = Template.PRE_PROFILE_VALUE;
                state = State.PROFILE_MATCHED_WITH_PRE_ID_VALUE;
            } else if (state == State.PROFILE_CANDIDATE_WITH_PRE_ID_PROPERTIES) {
                state = State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES;
            } else {
                state = State.PROFILE_MATCHED;
            }
        } else {
            state = State.PROFILE_UNMATCHED;
            clearPreIdProfileMarks();
        }
        // getElementText could've pushed the pointer to END_ELEMENT
        if (!pom.isEndElement()) {
            return state;
        }
        markStartWhileSearchingInsertionTarget();
        return state;
    }

    private State scanPropertiesBeforeProfileId(State state) throws XMLStreamException {
        State resultState = state == State.PROFILE_CANDIDATE_WITH_PRE_ID_VALUE
                ? State.PROFILE_CANDIDATE_WITH_PRE_ID_VALUE
                : State.PROFILE_CANDIDATE_WITH_PRE_ID_PROPERTIES;
        pom.mark(Marks.PRE_PROFILE_PROPERTY_START);
        boolean inTargetProperty = false;

        while (pom.hasNext()) {
            pom.next();

            if (pom.isStartElement()) {
                if (propertyName.equals(pom.getLocalName())) {
                    pom.mark(Marks.PRE_PROFILE_VALUE_START);
                    inTargetProperty = true;
                    continue;
                }
                pom.skipElement();
                continue;
            }

            if (pom.isEndElement()) {
                if (inTargetProperty && propertyName.equals(pom.getLocalName())) {
                    pom.mark(Marks.PRE_PROFILE_VALUE_END);
                    resultState = State.PROFILE_CANDIDATE_WITH_PRE_ID_VALUE;
                    inTargetProperty = false;
                    continue;
                }
                if ("properties".equals(pom.getLocalName())) {
                    pom.mark(Marks.PRE_PROFILE_PROPERTY_END);
                    return resultState;
                }
                continue;
            }

            if (!inTargetProperty) {
                pom.mark(Marks.PRE_PROFILE_PROPERTY_START);
            }
        }
        return resultState;
    }

    private void clearPreIdProfileMarks() {
        pom.clearMark(Marks.PRE_PROFILE_PROPERTY_START);
        pom.clearMark(Marks.PRE_PROFILE_PROPERTY_END);
        pom.clearMark(Marks.PRE_PROFILE_VALUE_START);
        pom.clearMark(Marks.PRE_PROFILE_VALUE_END);
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
            case PROFILE_CANDIDATE:
            case PROFILE_CANDIDATE_WITH_PRE_ID_PROPERTIES:
            case PROFILE_CANDIDATE_WITH_PRE_ID_VALUE:
            case PROFILE_MATCHED:
            case PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES:
            case PROFILE_MATCHED_WITH_PRE_ID_VALUE:
            case PROFILE_UNMATCHED:
                if ("profile".equals(name)) {
                    return handleEndProfile(state);
                }
                break;
            case PROJECT_PROPERTIES:
                if ("properties".equals(name)) {
                    return handleEndProjectProperties();
                }
                break;
            case PROFILE_MATCHED_PROPERTIES:
                if ("properties".equals(name)) {
                    return handleEndMatchedProfileProperties(State.PROFILE_MATCHED);
                }
                break;
            case PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTIES:
                if ("properties".equals(name)) {
                    return handleEndMatchedProfileProperties(State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES);
                }
                break;
            case PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTIES:
                if ("properties".equals(name)) {
                    return handleEndMatchedProfileProperties(State.PROFILE_MATCHED_WITH_PRE_ID_VALUE);
                }
                break;
            case PROJECT_PROPERTY:
                if (propertyName.equals(name)) {
                    return handleEndProperty(State.PROJECT_PROPERTIES);
                }
                break;
            case PROFILE_MATCHED_PROPERTY:
                if (propertyName.equals(name)) {
                    return handleEndProperty(State.PROFILE_MATCHED_PROPERTIES);
                }
                break;
            case PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTY:
                if (propertyName.equals(name)) {
                    return handleEndProperty(State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES_PROPERTIES);
                }
                break;
            case PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTY:
                if (propertyName.equals(name)) {
                    return handleEndProperty(State.PROFILE_MATCHED_WITH_PRE_ID_VALUE_PROPERTIES);
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

    private State handleEndProfile(State state) {
        if (state == State.PROFILE_MATCHED_WITH_PRE_ID_PROPERTIES || state == State.PROFILE_MATCHED_WITH_PRE_ID_VALUE) {
            if (result == Template.NONE) {
                result = Template.PRE_PROFILE_PROPERTY;
            }
        } else if (state == State.PROFILE_MATCHED) {
            if (result == Template.NONE) {
                pom.mark(Marks.END);
                result = Template.PROFILE_PROPERTIES;
            }
        } else {
            clearPreIdProfileMarks();
        }

        return State.PROFILES;
    }

    private State handleEndProjectProperties() {
        if (result == Template.NONE) {
            pom.mark(Marks.END);
            result = Template.PROJECT_PROPERTY;
        }
        return State.PROJECT;
    }

    private State handleEndMatchedProfileProperties(State returnState) {
        if (result == Template.NONE) {
            pom.mark(Marks.END);
            result = Template.PROFILE_PROPERTY;
        }
        return returnState;
    }

    private State handleEndProperty(State returnState) {
        pom.mark(Marks.VALUE_END);
        result = Template.VALUE;
        return returnState;
    }
}
