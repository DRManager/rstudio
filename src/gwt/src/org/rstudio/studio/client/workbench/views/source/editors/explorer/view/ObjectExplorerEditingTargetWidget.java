/*
 * ObjectExplorerEditingTargetWidget.java
 *
 * Copyright (C) 2009-17 by RStudio, Inc.
 *
 * Unless you have received this program directly from RStudio pursuant
 * to the terms of a commercial license agreement with RStudio, then
 * this program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
 *
 */
package org.rstudio.studio.client.workbench.views.source.editors.explorer.view;

import org.rstudio.studio.client.RStudioGinjector;
import org.rstudio.studio.client.workbench.views.source.editors.explorer.ObjectExplorerServerOperations;
import org.rstudio.studio.client.workbench.views.source.editors.explorer.model.ObjectExplorerHandle;
import com.google.gwt.user.client.ui.Composite;

public class ObjectExplorerEditingTargetWidget extends Composite
{
   public ObjectExplorerEditingTargetWidget(ObjectExplorerHandle handle)
   {
      RStudioGinjector.INSTANCE.injectMembers(this);
      
      handle_ = handle;
      
      // TODO: probably want to wrap this in e.g. a DockLayoutPanel
      // and give controls or similar in the header
      grid_ = new ObjectExplorerDataGrid(handle);
      initWidget(grid_);
   }
   
   public void onActivate()
   {
      // TODO
   }
   
   public void onDeactivate()
   {
      // TODO
   }
   
   private final ObjectExplorerHandle handle_;
   
   private final ObjectExplorerDataGrid grid_;
   
   // Injected ----
   private ObjectExplorerServerOperations server_;
}
