package org.codehaus.mojo.versions;

import org.apache.commons.lang.StringUtils;

/**
 * Store that holds project version. It is needed because multiple plugin goals can run at once.
 *
 * @author <a href="mailto:petr.ujezdsky@gmail.com">Petr Újezdský</a>
 * @version $Id$
 */
public class VersionStore {

    private static String version;

    public static void setVersionIfEmpty(String version) {
        if (StringUtils.isBlank(VersionStore.version)) {
            VersionStore.version = version;
        }
    }

    public static void setVersion(String version) {
        VersionStore.version = version;
    }

    public static String getVersion() {
        return version;
    }
}
