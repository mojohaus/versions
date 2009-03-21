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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.apache.maven.wagon.proxy.ProxyInfo;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.UnsupportedProtocolException;
import org.apache.maven.wagon.ConnectionException;
import org.apache.maven.wagon.observers.Debug;
import org.apache.maven.wagon.repository.Repository;
import org.apache.maven.wagon.authentication.AuthenticationException;
import org.apache.maven.settings.Settings;
import org.apache.maven.settings.Proxy;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.manager.WagonConfigurationException;
import org.apache.maven.plugin.logging.Log;

/**
 * Utility methods to help with using {@link Wagon}s.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public final class WagonUtils
{
    private WagonUtils()
    {
        throw new IllegalAccessError( "Utility classes should never be instantiated" );
    }

    /**
     * Convenience method to convert the {@link org.apache.maven.settings.Proxy} object from a {@link org.apache.maven.settings.Settings} into a {@link org.apache.maven.wagon.proxy.ProxyInfo}.
     *
     * @param settings The settings to use.
     * @return The proxy details from the settings or <code>null</code> if the settings do not define a proxy.
     */
    public static ProxyInfo getProxyInfo( Settings settings )
    {
        ProxyInfo proxyInfo = null;
        if ( settings != null && settings.getActiveProxy() != null )
        {
            proxyInfo = new ProxyInfo();
            final Proxy proxy = settings.getActiveProxy();
            proxyInfo.setHost( proxy.getHost() );
            proxyInfo.setType( proxy.getProtocol() );
            proxyInfo.setPort( proxy.getPort() );
            proxyInfo.setNonProxyHosts( proxy.getNonProxyHosts() );
            proxyInfo.setUserName( proxy.getUsername() );
            proxyInfo.setPassword( proxy.getPassword() );
        }
        return proxyInfo;
    }

    /**
     * Convenience method to create a wagon.
     *
     * @param serverId     The serverId to use if the wagonManager needs help.
     * @param url          The url to create a wagon for.
     * @param wagonManager The wgaon manager to use.
     * @param settings     The settings to use.
     * @param logger       The logger to use.
     * @return The wagon to connect to the url.
     * @throws org.apache.maven.wagon.UnsupportedProtocolException
     *          if the protocol is not supported.
     * @throws org.apache.maven.artifact.manager.WagonConfigurationException
     *          if the wagon cannot be configured.
     * @throws org.apache.maven.wagon.ConnectionException
     *          If the connection cannot be established.
     * @throws org.apache.maven.wagon.authentication.AuthenticationException
     *          If the connection cannot be authenticated.
     */
    public static Wagon createWagon( String serverId, String url, WagonManager wagonManager, Settings settings,
                                     Log logger )
        throws UnsupportedProtocolException, WagonConfigurationException, ConnectionException, AuthenticationException
    {
        Repository repository = new Repository( serverId, url );
        Wagon wagon = wagonManager.getWagon( repository );

        if ( logger.isDebugEnabled() )
        {
            Debug debug = new Debug();
            wagon.addSessionListener( debug );
            wagon.addTransferListener( debug );
        }

        ProxyInfo proxyInfo = getProxyInfo( settings );
        if ( proxyInfo != null )
        {
            wagon.connect( repository, wagonManager.getAuthenticationInfo( repository.getId() ), proxyInfo );
        }
        else
        {
            wagon.connect( repository, wagonManager.getAuthenticationInfo( repository.getId() ) );
        }
        return wagon;
    }
}
