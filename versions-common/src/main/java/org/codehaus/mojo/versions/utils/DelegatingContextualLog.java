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

import java.util.Objects;

import org.apache.maven.plugin.logging.Log;

/**
 * A {@link ContextualLog} implementation that delegates to a given {@link Log}.
 * <p>
 * This class provides a way to add contextual information to log messages by
 * maintaining a current context. When a context is set, the first log message
 * at each level (debug, info, warn, error) will be prefixed with the context.
 * When the context is cleared, a blank line is logged at the highest level
 * that was used during the context.
 * </p>
 * <p>
 * This is useful for grouping log messages related to a specific operation or
 * component, making it easier to identify and understand the context of log
 * entries.
 * </p>
 *
 * @author Stephen Connolly
 */
public class DelegatingContextualLog implements ContextualLog {
    private final Log delegate;

    private String currentContext = null;

    private boolean currentContextReportedDebug = false;

    private boolean currentContextReportedInfo = false;

    private boolean currentContextReportedWarn = false;

    private boolean currentContextReportedError = false;

    /**
     * Creates a new {@link DelegatingContextualLog} instance.
     *
     * @param delegate the delegate log
     */
    public DelegatingContextualLog(Log delegate) {
        this.delegate = delegate;
    }

    public synchronized void setContext(String context) {
        if (Objects.equals(currentContext, context)) {
            return;
        }
        if (currentContext != null) {
            clearContext();
        }
        currentContext = context;
        currentContextReportedDebug = false;
        currentContextReportedInfo = false;
        currentContextReportedWarn = false;
        currentContextReportedError = false;
    }

    public synchronized void clearContext() {
        if (currentContextReportedError) {
            delegate.error("");
        } else if (currentContextReportedWarn) {
            delegate.warn("");
        } else if (currentContextReportedInfo) {
            delegate.info("");
        } else if (currentContextReportedDebug) {
            delegate.debug("");
        }
        currentContextReportedDebug = false;
        currentContextReportedInfo = false;
        currentContextReportedWarn = false;
        currentContextReportedError = false;
    }

    private void enterContextDebug() {
        if (!currentContextReportedDebug
                && !currentContextReportedInfo
                && !currentContextReportedWarn
                && !currentContextReportedError) {
            delegate.debug(currentContext);
            currentContextReportedDebug = true;
        }
    }

    private void enterContextInfo() {
        if (!currentContextReportedInfo && !currentContextReportedWarn && !currentContextReportedError) {
            delegate.info(currentContext);
            currentContextReportedInfo = true;
        }
    }

    private void enterContextWarn() {
        if (!currentContextReportedWarn && !currentContextReportedError) {
            delegate.warn(currentContext);
            currentContextReportedWarn = true;
        }
    }

    private void enterContextError() {
        if (!currentContextReportedError) {
            delegate.error(currentContext);
            currentContextReportedError = true;
        }
    }

    public void debug(CharSequence charSequence) {
        enterContextDebug();
        delegate.debug(charSequence);
    }

    public void debug(CharSequence charSequence, Throwable throwable) {
        enterContextDebug();
        delegate.debug(charSequence, throwable);
    }

    public void debug(Throwable throwable) {
        enterContextDebug();
        delegate.debug(throwable);
    }

    public void info(CharSequence charSequence) {
        enterContextInfo();
        delegate.info(charSequence);
    }

    public void info(CharSequence charSequence, Throwable throwable) {
        enterContextInfo();
        delegate.info(charSequence, throwable);
    }

    public void info(Throwable throwable) {
        enterContextInfo();
        delegate.info(throwable);
    }

    public void warn(CharSequence charSequence) {
        enterContextWarn();
        delegate.warn(charSequence);
    }

    public void warn(CharSequence charSequence, Throwable throwable) {
        enterContextWarn();
        delegate.warn(charSequence, throwable);
    }

    public void warn(Throwable throwable) {
        enterContextWarn();
        delegate.warn(throwable);
    }

    public void error(CharSequence charSequence) {
        enterContextError();
        delegate.error(charSequence);
    }

    public void error(CharSequence charSequence, Throwable throwable) {
        enterContextError();
        delegate.error(charSequence, throwable);
    }

    public void error(Throwable throwable) {
        enterContextError();
        delegate.error(throwable);
    }

    public boolean isDebugEnabled() {
        return delegate.isDebugEnabled();
    }

    public boolean isInfoEnabled() {
        return delegate.isInfoEnabled();
    }

    public boolean isWarnEnabled() {
        return delegate.isWarnEnabled();
    }

    public boolean isErrorEnabled() {
        return delegate.isErrorEnabled();
    }
}
