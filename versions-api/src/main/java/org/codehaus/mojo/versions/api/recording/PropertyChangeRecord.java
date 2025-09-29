package org.codehaus.mojo.versions.api.recording;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import org.codehaus.mojo.versions.api.change.PropertyVersionChange;

/**
 * Represents a change record of an item's version.
 *
 * @author Slawomir Jaranowski
 * @since 2.14.0
 * @deprecated use classes from {@code versions-model}
 */
@Deprecated
public interface PropertyChangeRecord extends ChangeRecord<PropertyVersionChange> {}
