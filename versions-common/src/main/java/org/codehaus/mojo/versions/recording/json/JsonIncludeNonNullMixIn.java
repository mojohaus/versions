package org.codehaus.mojo.versions.recording.json;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Mix-in to instruct Jackson to omit null-valued properties during serialization.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public abstract class JsonIncludeNonNullMixIn {}
