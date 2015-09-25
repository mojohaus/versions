package org.codehaus.mojo.versions;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

/**
 * Store that holds project version. It is needed because multiple plugin goals can run at once.
 *
 * @author <a href="mailto:petr.ujezdsky@gmail.com">Petr Újezdský</a>
 * @version $Id$
 */
public class VersionStore {

    private static Map<String, String> versionMap = new HashMap<String, String>();

    public static void setVersionIfEmpty(String artifactId, String version) {
        if (StringUtils.isBlank(VersionStore.versionMap.get(artifactId))) {
            VersionStore.versionMap.put(artifactId, version);
        }
    }

    public static void setVersion(String artifactId, String version) {
        VersionStore.versionMap.put(artifactId, version);
    }

    public static String getVersion(String artifactId) {
        return VersionStore.versionMap.get(artifactId);
    }
}
