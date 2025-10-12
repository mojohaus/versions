package org.codehaus.mojo.versions.api;

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

import java.util.Optional;

import org.apache.maven.artifact.Artifact;

/**
 * Exception thrown if version information cannot be retrieved.
 */
public class VersionRetrievalException extends Exception {

    /** The artifact for which version retrieval failed, may be {@code null}. */
    private final Artifact artifact;

    /**
     * Constructs a new exception with {@code null} as its detail message.
     * The cause is not initialized, and may subsequently be initialized by a
     * call to {@link #initCause}.
     *
     * @param artifact {@link Artifact} instance causing the problem
     */
    public VersionRetrievalException(Artifact artifact) {
        this(null, artifact);
    }

    /**
     * Constructs a new exception with the specified detail message.  The
     * cause is not initialized, and may subsequently be initialized by
     * a call to {@link #initCause}.
     *
     * @param artifact {@link Artifact} instance causing the problem
     * @param   message   the detail message. The detail message is saved for
     *          later retrieval by the {@link #getMessage()} method.
     */
    public VersionRetrievalException(String message, Artifact artifact) {
        super(message);
        this.artifact = artifact;
    }

    /**
     * Constructs a new exception with the specified cause and a detail
     * message of {@code (cause==null ? null : cause.toString())} (which
     * typically contains the class and detail message of {@code cause}).
     * This constructor is useful for exceptions that are little more than
     * wrappers for other throwables (for example, {@link
     * java.security.PrivilegedActionException}).
     *
     * @param artifact {@link Artifact} instance causing the problem
     * @param  cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A {@code null} value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     */
    public VersionRetrievalException(Artifact artifact, Throwable cause) {
        this(null, artifact, cause);
    }

    /**
     * Constructs a new exception with the specified detail message and
     * cause.  <p>Note that the detail message associated with
     * {@code cause} is <i>not</i> automatically incorporated in
     * this exception's detail message.
     *
     * @param  message the detail message (which is saved for later retrieval
     *         by the {@link #getMessage()} method).
     * @param artifact {@link Artifact} instance causing the problem
     * @param  cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A {@code null} value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     */
    public VersionRetrievalException(String message, Artifact artifact, Throwable cause) {
        super(message, cause);
        this.artifact = artifact;
    }

    /**
     * Returns the artifact causing the problem with version retrieval, if available
     * @return {@link Optional} object containing artifact causing the problem with version retrieval, or empty
     */
    public Optional<Artifact> getArtifact() {
        return Optional.ofNullable(artifact);
    }
}
