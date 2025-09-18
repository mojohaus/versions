package org.codehaus.mojo.versions;

import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import org.apache.maven.wagon.ResourceDoesNotExistException;
import org.apache.maven.wagon.TransferFailedException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.authorization.AuthorizationException;
import org.apache.maven.wagon.events.SessionListener;
import org.apache.maven.wagon.events.TransferListener;
import org.apache.maven.wagon.proxy.ProxyInfo;
import org.apache.maven.wagon.proxy.ProxyInfoProvider;
import org.apache.maven.wagon.repository.Repository;

class TestWagonStub implements Wagon {
    private final Consumer<File> getter;

    TestWagonStub(Consumer<File> getter) {
        this.getter = getter;
    }

    @Override
    public void get(String resourceName, File destination)
            throws TransferFailedException, ResourceDoesNotExistException, AuthorizationException {
        getter.accept(destination);
    }

    @Override
    public boolean getIfNewer(String resourceName, File destination, long timestamp) {
        return false;
    }

    @Override
    public void put(File source, String destination)
            throws TransferFailedException, ResourceDoesNotExistException, AuthorizationException {}

    @Override
    public void putDirectory(File sourceDirectory, String destinationDirectory) {}

    @Override
    public boolean resourceExists(String resourceName) {
        return false;
    }

    @Override
    public List<String> getFileList(String destinationDirectory) {
        return Collections.emptyList();
    }

    @Override
    public boolean supportsDirectoryCopy() {
        return false;
    }

    @Override
    public Repository getRepository() {
        return null;
    }

    @Override
    public void connect(Repository source) {}

    @Override
    public void connect(Repository source, ProxyInfo proxyInfo) {}

    @Override
    public void connect(Repository source, ProxyInfoProvider proxyInfoProvider) {}

    @Override
    public void connect(Repository source, AuthenticationInfo authenticationInfo) {}

    @Override
    public void connect(Repository source, AuthenticationInfo authenticationInfo, ProxyInfo proxyInfo) {}

    @Override
    public void connect(
            Repository source, AuthenticationInfo authenticationInfo, ProxyInfoProvider proxyInfoProvider) {}

    @Override
    public void openConnection() {}

    @Override
    public void disconnect() {}

    @Override
    public void setTimeout(int timeoutValue) {}

    @Override
    public int getTimeout() {
        return 0;
    }

    @Override
    public void setReadTimeout(int timeoutValue) {}

    @Override
    public int getReadTimeout() {
        return 0;
    }

    @Override
    public void addSessionListener(SessionListener listener) {}

    @Override
    public void removeSessionListener(SessionListener listener) {}

    @Override
    public boolean hasSessionListener(SessionListener listener) {
        return false;
    }

    @Override
    public void addTransferListener(TransferListener listener) {}

    @Override
    public void removeTransferListener(TransferListener listener) {}

    @Override
    public boolean hasTransferListener(TransferListener listener) {
        return false;
    }

    @Override
    public boolean isInteractive() {
        return false;
    }

    @Override
    public void setInteractive(boolean interactive) {}
}
