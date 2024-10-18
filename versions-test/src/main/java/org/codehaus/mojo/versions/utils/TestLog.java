package org.codehaus.mojo.versions.utils;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.tuple.Triple;
import org.apache.maven.plugin.logging.Log;

public class TestLog implements Log {
    private final List<Triple<LogLevel, CharSequence, Throwable>> loggedMessages = new ArrayList<>();

    public List<Triple<LogLevel, CharSequence, Throwable>> getLoggedMessages() {
        return loggedMessages;
    }

    @Override
    public boolean isDebugEnabled() {
        return false;
    }

    @Override
    public void debug(CharSequence charSequence) {
        debug(charSequence, null);
    }

    @Override
    public void debug(CharSequence charSequence, Throwable throwable) {
        loggedMessages.add(Triple.of(LogLevel.DEBUG, charSequence, throwable));
    }

    @Override
    public void debug(Throwable throwable) {
        debug(null, throwable);
    }

    @Override
    public boolean isInfoEnabled() {
        return false;
    }

    @Override
    public void info(CharSequence charSequence) {
        info(charSequence, null);
    }

    @Override
    public void info(CharSequence charSequence, Throwable throwable) {
        loggedMessages.add(Triple.of(LogLevel.INFO, charSequence, throwable));
    }

    @Override
    public void info(Throwable throwable) {
        info(null, throwable);
    }

    @Override
    public boolean isWarnEnabled() {
        return false;
    }

    @Override
    public void warn(CharSequence charSequence) {
        warn(charSequence, null);
    }

    @Override
    public void warn(CharSequence charSequence, Throwable throwable) {
        loggedMessages.add(Triple.of(LogLevel.WARN, charSequence, throwable));
    }

    @Override
    public void warn(Throwable throwable) {
        warn(null, throwable);
    }

    @Override
    public boolean isErrorEnabled() {
        return false;
    }

    @Override
    public void error(CharSequence charSequence) {
        error(charSequence, null);
    }

    @Override
    public void error(CharSequence charSequence, Throwable throwable) {
        loggedMessages.add(Triple.of(LogLevel.ERROR, charSequence, throwable));
    }

    @Override
    public void error(Throwable throwable) {
        error(null, throwable);
    }

    public enum LogLevel {
        DEBUG,
        INFO,
        WARN,
        ERROR
    }
}
