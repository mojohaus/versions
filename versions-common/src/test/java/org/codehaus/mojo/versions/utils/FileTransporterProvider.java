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

import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.spi.connector.transport.Transporter;
import org.eclipse.aether.spi.connector.transport.TransporterProvider;
import org.eclipse.aether.transfer.NoTransporterException;
import org.eclipse.aether.transport.file.FileTransporterFactory;

/**
 * A {@link TransporterProvider} for tests which serves {@code file:} repositories with Maven Resolver's
 * real file transport and refuses every other protocol.
 */
public class FileTransporterProvider implements TransporterProvider {
    private final FileTransporterFactory factory = new FileTransporterFactory();

    @Override
    public Transporter newTransporter(RepositorySystemSession session, RemoteRepository repository)
            throws NoTransporterException {
        return factory.newInstance(session, repository);
    }
}
