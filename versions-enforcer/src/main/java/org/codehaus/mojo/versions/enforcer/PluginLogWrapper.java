package org.codehaus.mojo.versions.enforcer;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.apache.maven.enforcer.rule.api.EnforcerLogger;
import org.apache.maven.plugin.logging.Log;

/**
 * Wrapper used to pass {@link EnforcerLogger} as Maven plugin {@link Log}.
 */
class PluginLogWrapper implements Log {

    private final EnforcerLogger logger;

    PluginLogWrapper(EnforcerLogger logger) {
        this.logger = logger;
    }

    @Override
    public boolean isDebugEnabled() {
        return logger.isDebugEnabled();
    }

    @Override
    public void debug(CharSequence charSequence) {
        logger.debug(charSequence);
    }

    @Override
    public void debug(CharSequence charSequence, Throwable throwable) {
        logger.debug(charSequence + throwableToString(throwable));
    }

    @Override
    public void debug(Throwable throwable) {
        logger.debug(throwableToString(throwable));
    }

    @Override
    public boolean isInfoEnabled() {
        return logger.isInfoEnabled();
    }

    @Override
    public void info(CharSequence charSequence) {
        logger.info(charSequence);
    }

    @Override
    public void info(CharSequence charSequence, Throwable throwable) {

        logger.info(charSequence + throwableToString(throwable));
    }

    @Override
    public void info(Throwable throwable) {
        logger.info(throwableToString(throwable));
    }

    @Override
    public boolean isWarnEnabled() {
        return logger.isWarnEnabled();
    }

    @Override
    public void warn(CharSequence charSequence) {
        logger.warn(charSequence);
    }

    @Override
    public void warn(CharSequence charSequence, Throwable throwable) {
        logger.warn(charSequence + throwableToString(throwable));
    }

    @Override
    public void warn(Throwable throwable) {
        logger.warn(throwableToString(throwable));
    }

    @Override
    public boolean isErrorEnabled() {
        return logger.isErrorEnabled();
    }

    @Override
    public void error(CharSequence charSequence) {
        logger.error(charSequence);
    }

    @Override
    public void error(CharSequence charSequence, Throwable throwable) {
        logger.error(charSequence + throwableToString(throwable));
    }

    @Override
    public void error(Throwable throwable) {
        logger.error(throwableToString(throwable));
    }

    private String throwableToString(Throwable throwable) {
        if (throwable != null) {
            ByteArrayOutputStream stream = new ByteArrayOutputStream();
            throwable.printStackTrace(new PrintStream(stream));
            return System.lineSeparator() + stream;
        }
        return "";
    }
}
