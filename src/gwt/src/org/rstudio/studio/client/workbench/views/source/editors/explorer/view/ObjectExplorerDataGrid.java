/*
 * ObjectExplorerDataGrid.java
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

import java.util.ArrayList;
import java.util.List;

import org.rstudio.core.client.CommandWithArg;
import org.rstudio.core.client.Debug;
import org.rstudio.core.client.SafeHtmlUtil;
import org.rstudio.core.client.StringUtil;
import org.rstudio.core.client.dom.DomUtils;
import org.rstudio.core.client.js.JsUtil;
import org.rstudio.core.client.resources.ImageResource2x;
import org.rstudio.core.client.theme.RStudioDataGridResources;
import org.rstudio.core.client.theme.RStudioDataGridStyle;
import org.rstudio.core.client.theme.res.ThemeResources;
import org.rstudio.studio.client.RStudioGinjector;
import org.rstudio.studio.client.server.ServerError;
import org.rstudio.studio.client.server.ServerRequestCallback;
import org.rstudio.studio.client.workbench.views.source.editors.explorer.ObjectExplorerServerOperations;

/*
 * This widget provides a tabular, drill-down view into an R object.
 *
 * ## Columns
 *
 * ### Name
 *
 * The name column contains three elements:
 *
 *    1) An (optional) 'drill-down' icon, that expands the node such that
 *       children of that object are shown and added to the table,
 *
 *    2) An icon, denoting the object's type (list, environment, etc.)
 *
 *    3) The object's name; that is, the binding through which is can be accessed
 *       from the parent object.
 *
 * ### Type
 *
 * A text column, giving a short description of the object's type. Typically, this
 * will be the object's class, alongside the object's length (if relevant).
 *
 * ### Value
 *
 * A short, succinct description of the value of the object within.
 *
 * ### Size
 *
 * The amount of memory occupied by this object.
 *
 * ### Inspect
 *
 * A set of one or more widgets, used to e.g. open data.frames in the Data Viewer,
 * to find functions, and so on.
 */

import org.rstudio.studio.client.workbench.views.source.editors.explorer.model.ObjectExplorerHandle;
import org.rstudio.studio.client.workbench.views.source.editors.explorer.model.ObjectExplorerInspectionResult;
import org.rstudio.studio.client.workbench.views.source.editors.explorer.view.ObjectExplorerDataGrid.Data.ExpansionState;

import com.google.gwt.cell.client.AbstractCell;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.dom.client.Element;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.safehtml.shared.SafeHtmlBuilder;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.DataGrid;
import com.google.gwt.user.cellview.client.TextColumn;
import com.google.gwt.user.cellview.client.TextHeader;
import com.google.gwt.user.client.Event;
import com.google.gwt.view.client.CellPreviewEvent;
import com.google.gwt.view.client.CellPreviewEvent.Handler;
import com.google.gwt.view.client.ListDataProvider;
import com.google.inject.Inject;

public class ObjectExplorerDataGrid
      extends DataGrid<ObjectExplorerDataGrid.Data>
      implements ClickHandler,
      Handler<ObjectExplorerDataGrid.Data>
{
   public static class Data extends ObjectExplorerInspectionResult
   {
      protected Data()
      {
      }
      
      public final ExpansionState getExpansionState()
      {
         String state = getExpansionStateImpl();
         return ExpansionState.valueOf(state.toUpperCase());
      }
      
      private final native String getExpansionStateImpl()
      /*-{
         return this["expansion_state"] || "closed";
      }-*/;
      
      public final void setExpansionState(ExpansionState state)
      {
         setExpansionStateImpl(state.name());
      }
      
      private final native void setExpansionStateImpl(String state)
      /*-{
         this["expansion_state"] = state;
      }-*/;
      
      private final native boolean isVisible()
      /*-{
         var visible = this["visible"];
         if (typeof visible === "undefined")
            return true;
         return visible;
      }-*/;
      
      public final native void setVisible(boolean visible)
      /*-{
         this["visible"] = visible;
      }-*/;
      
      public final native Data getParentData()
      /*-{
         return this["parent"] || null;
      }-*/;
      
      public final native void setParentData(Data data)
      /*-{
         this["parent"] = data;
      }-*/;
      
      public final native boolean hasParentData(Data data)
      /*-{
         
         for (var parent = this["parent"];
              parent != null;
              parent = parent["parent"])
         {
            if (parent == data)
               return true;
         }
         
         return false;
         
      }-*/;
      
      public final native JsArray<Data> getChildrenData()
      /*-{
         return this["children"] || null;
      }-*/;
      
      public final native void setChildrenData(JsArray<Data> data)
      /*-{
         this["children"] = data;
      }-*/;
      
      public final int getDepth()
      {
         int depth = 0;
         for (Data parent = getParentData();
              parent != null;
              parent = parent.getParentData())
         {
            depth++;
         }
         
         return depth;
      }
      
      public final void updateChildOwnership()
      {
         updateChildOwnership(getChildrenData(), this);
      }
      
      private static final void updateChildOwnership(JsArray<Data> children,
                                                     Data parent)
      {
         if (children == null)
            return;

         for (int i = 0, n = children.length(); i < n; i++)
         {
            // update parent data on this child
            Data child = children.get(i);
            child.setParentData(parent);
            
            // recurse
            updateChildOwnership(child.getChildrenData(), child);
         }
      }
      
      public final native void updateChildOwnershipImpl()
      /*-{
         var children = this["children"] || [];
         for (var i = 0, n = children.length; i < n; i++)
            children[i].parent = this;
         
      }-*/;
      
      public enum ExpansionState
      {
         OPEN,
         CLOSED
      }
   }
   
   private static interface Filter<T>
   {
      public boolean accept(T data);
   }
   
   private static class NameCell
         extends AbstractCell<Data>
   {
      @Override
      public void render(Context context,
                         Data data,
                         SafeHtmlBuilder builder)
      {
         builder.appendHtmlConstant("<table>");
         builder.appendHtmlConstant("<tr>");
         
         builder.appendHtmlConstant("<td>");
         addIndent(builder, data);
         builder.appendHtmlConstant("</td>");
         
         builder.appendHtmlConstant("<td width=20>");
         addExpandIcon(builder, data);
         builder.appendHtmlConstant("</td>");
         
         builder.appendHtmlConstant("<td width=20>");
         addIcon(builder, data);
         builder.appendHtmlConstant("</td>");
         
         builder.appendHtmlConstant("<td>");
         addName(builder, data);
         builder.appendHtmlConstant("</td>");
         
         builder.appendHtmlConstant("</tr>");
         builder.appendHtmlConstant("</table>");
      }
      
      private static final void addIndent(SafeHtmlBuilder builder, Data data)
      {
         int indentPx = data.getDepth() * 8;
         if (indentPx == 0)
            return;
         
         String html = "<div style='width: " + indentPx + "px'></div>";
         builder.appendHtmlConstant(html);
      }
      
      private static final void addExpandIcon(SafeHtmlBuilder builder, Data data)
      {
         // non-recursive objects aren't expandable
         if (!data.isRecursive())
            return;
         
         // add expand button
         switch (data.getExpansionState())
         {
         case CLOSED:
            builder.append(SafeHtmlUtil.createOpenTag("div",
                  "class", "expandButton",
                  "data-action", ACTION_OPEN));
            builder.append(IMAGE_RIGHT_ARROW.getSafeHtml());
            builder.appendHtmlConstant("</div>");
            break;
         case OPEN:
            builder.append(SafeHtmlUtil.createOpenTag("div",
                  "class", "expandButton",
                  "data-action", ACTION_CLOSE));
            builder.append(IMAGE_DOWN_ARROW.getSafeHtml());
            builder.appendHtmlConstant("</div>");
            break;
         }
      }
      
      private static final void addIcon(SafeHtmlBuilder builder,
                                        ObjectExplorerInspectionResult result)
      {
         // TODO: icon based on type
         builder.append(SafeHtmlUtil.createOpenTag("div"));
         builder.append(IMAGE_TYPE_DATA.getSafeHtml());
         builder.appendHtmlConstant("</div>");
      }
      
      private static final void addName(SafeHtmlBuilder builder,
                                        ObjectExplorerInspectionResult result)
      {
         String name = result.getObjectName();
         if (name == null)
            name = "<unknown>";
         builder.appendEscaped(name);
      }
   }
   
   public ObjectExplorerDataGrid(ObjectExplorerHandle handle)
   {
      super(1000, RES);
      handle_ = handle;
      
      RStudioGinjector.INSTANCE.injectMembers(this);
      
      setSize("100%", "100%");
      
      // add columns
      nameColumn_ = new Column<Data, Data>(new NameCell())
      {
         @Override
         public Data getValue(Data data)
         {
            return data;
         }
      };
      addColumn(nameColumn_, new TextHeader("Name"));
      
      typeColumn_ = new TextColumn<Data>()
      {
         @Override
         public String getValue(Data data)
         {
            return data.getObjectType();
         }
      };
      addColumn(typeColumn_, new TextHeader("Type"));
      setColumnWidth(typeColumn_, "20%");
      
      valueColumn_ = new TextColumn<Data>()
      {
         @Override
         public String getValue(Data data)
         {
            return data.getObjectDescription();
         }
      };
      addColumn(valueColumn_, new TextHeader("Value"));
      setColumnWidth(valueColumn_, "40%");
      
      sizeColumn_ = new TextColumn<Data>()
      {
         @Override
         public String getValue(Data data)
         {
            // TODO: use actual size
            return data.getObjectId();
         }
      };
      addColumn(sizeColumn_, new TextHeader("Size"));
      setColumnWidth(sizeColumn_, "20%");
      
      // set updater
      dataProvider_ = new ListDataProvider<Data>();
      dataProvider_.setList(new ArrayList<Data>());;
      dataProvider_.addDataDisplay(this);
      
      // register handlers
      setKeyboardSelectionHandler(this);
      addDomHandler(this, ClickEvent.getType());
      
      // populate the view once initially
      initializeRoot();
   }
   
   @Inject
   private void initialize(ObjectExplorerServerOperations server)
   {
      server_ = server;
   }
   
   // Handlers ---
   
   @Override
   public void onClick(ClickEvent event)
   {
      // extract target element
      Element targetEl = event.getNativeEvent().getEventTarget().cast();
      if (targetEl == null)
         return;
      
      // determine action associated with this row
      Element dataEl = DomUtils.findParentElement(targetEl, new DomUtils.ElementPredicate()
      {
         @Override
         public boolean test(Element el)
         {
            return el.hasAttribute("data-action");
         }
      });
      
      // find associated row index by looking up through the DOM
      Element rowEl = DomUtils.findParentElement(targetEl, new DomUtils.ElementPredicate()
      {
         @Override
         public boolean test(Element el)
         {
            return el.hasAttribute("__gwt_row");
         }
      });
      
      if (rowEl == null)
         return;
      
      int row = StringUtil.parseInt(rowEl.getAttribute("__gwt_row"), -1);
      if (row == -1)
         return;
      
      // if the user has clicked on the expand button, handle that
      if (dataEl != null)
      {
         // perform action
         String action = dataEl.getAttribute("data-action");
         performAction(action, row);
         return;
      }
      
      // otherwise, just select the row the user clicked on
      setKeyboardSelectedRow(row);
      setKeyboardSelectedColumn(0);
   }
   
   @Override
   public void onCellPreview(CellPreviewEvent<Data> preview)
   {
      Event event = Event.getCurrentEvent();
      int code = event.getKeyCode();
      int type = event.getTypeInt();
      int row = getKeyboardSelectedRow();
      boolean isDefault = false;
      
      if (type == Event.ONKEYDOWN || type == Event.ONKEYPRESS)
      {
         switch (code)
         {
         case KeyCodes.KEY_UP:
            selectRowRelative(-1);
            break;

         case KeyCodes.KEY_DOWN:
            selectRowRelative(+1);
            break;

         case KeyCodes.KEY_PAGEUP:
            selectRowRelative(-10);
            break;

         case KeyCodes.KEY_PAGEDOWN:
            selectRowRelative(+10);
            break;

         case KeyCodes.KEY_LEFT:
            selectParentOrClose(row);
            break;

         case KeyCodes.KEY_RIGHT:
            selectChildOrOpen(row);
            break;
            
         default:
            isDefault = true;
            break;
         }
      }
      
      else if (type == Event.ONKEYUP)
      {
         switch (code)
         {
         case KeyCodes.KEY_ENTER:
         case KeyCodes.KEY_SPACE:
            toggleExpansion(row);
            break;

         default:
            isDefault = true;
            break;
         }
      }
      else
      {
         isDefault = true;
      }
      
      // eat any non-default handled events
      if (!isDefault)
      {
         preview.setCanceled(true);
         event.stopPropagation();
         event.preventDefault();
      }
   }
   
   // Private Methods ----
   
   private void selectRowRelative(int delta)
   {
      setKeyboardSelectedColumn(0);
      setKeyboardSelectedRow(getKeyboardSelectedRow() + delta);
   }
   
   private void selectParentOrClose(int row)
   {
      Data data = getData().get(row);
      
      // if this node has children and is currently expanded, close it
      if (data.isRecursive() && data.getExpansionState() == ExpansionState.OPEN)
      {
         closeRow(row);
         return;
      }
      
      // otherwise, select the parent associated with this row (if any)
      Data parent = data.getParentData();
      if (parent == null)
         return;
      
      List<Data> list = getData();
      for (int i = 0, n = row; i < n; i++)
      {
         if (list.get(i).equals(parent))
         {
            setKeyboardSelectedRow(i);
            break;
         }
      }
   }
   
   private void selectChildOrOpen(int row)
   {
      Data data = getData().get(row);
      
      // if this node has children but is not expanded, expand it
      if (data.isRecursive() && data.getExpansionState() == ExpansionState.CLOSED)
      {
         openRow(row);
         return;
      }
      
      // otherwise, select the first child of this row (the next row)
      selectRowRelative(1);
   }
   
   private void toggleExpansion(int row)
   {
      Data data = getData().get(row);
      
      switch (data.getExpansionState())
      {
      case OPEN:
         closeRow(row);
         break;
      case CLOSED:
         openRow(row);
         break;
      }
   }
   
   private void performAction(String action, int row)
   {
      if (action.equals(ACTION_OPEN))
      {
         openRow(row);
      }
      else if (action.equals(ACTION_CLOSE))
      {
         closeRow(row);
      }
      else
      {
         assert false : "Unexpected action '" + action + "' on row " + row;
      }
   }
   
   private void openRow(final int row)
   {
      final Data data = getData().get(row);
      Debug.logToRConsole("Opening row for object:" + data.getObjectId());
      
      // if this is a non-recursive object, error (shouldn't be
      // called in that case)
      if (!data.isRecursive())
         assert false: "Attempted to expand non-recursive row " + row;
      
      // toggle expansion state
      data.setExpansionState(ExpansionState.OPEN);
      
      // resolve children and show
      withChildren(data, new CommandWithArg<JsArray<Data>>()
      {
         @Override
         public void execute(JsArray<Data> children)
         {
            // set all direct children as visible
            for (int i = 0, n = children.length(); i < n; i++)
               children.get(i).setVisible(true);
            
            // force update of data grid
            synchronize();
         }
      });
   }
   
   private void closeRow(int row)
   {
      final Data data = getData().get(row);
      Debug.logToRConsole("Closing row for object:" + data.getObjectId());
      
      // if this is a non-recursive object, error (shouldn't be
      // called in that case)
      if (!data.isRecursive())
         assert false: "Attempted to close non-recursive row " + row;
      
      // toggle expansion state
      data.setExpansionState(ExpansionState.CLOSED);
      
      // set direct children as non-visible
      withChildren(data, new CommandWithArg<JsArray<Data>>()
      {
         @Override
         public void execute(JsArray<Data> children)
         {
            // set all direct children as invisible
            for (int i = 0, n = children.length(); i < n; i++)
               children.get(i).setVisible(false);
            
            // force update of data grid
            synchronize();
         }
      });
      
   }
   
   private void withChildren(final Data data,
                             final CommandWithArg<JsArray<Data>> command)
   {
      // if we already have children, use them
      JsArray<Data> children = data.getChildrenData();
      if (children != null)
      {
         command.execute(children);
         return;
      }
      
      // no children; make a server RPC request and then call back
      server_.explorerInspectChildren(
            data.getObjectId(),
            new ServerRequestCallback<JsArray<ObjectExplorerInspectionResult>>()
            {
               @Override
               public void onResponseReceived(JsArray<ObjectExplorerInspectionResult> children)
               {
                  // connect data with associated children
                  JsArray<Data> childrenData = children.cast();
                  data.setChildrenData(childrenData);
                  for (int i = 0, n = children.length(); i < n; i++)
                     childrenData.get(i).setParentData(data);
                  
                  // execute command with children
                  command.execute(childrenData);
               }
               
               @Override
               public void onError(ServerError error)
               {
                  Debug.logError(error);
               }
            });
   }
   
   private void initializeRoot()
   {
      server_.explorerInspectObject(
            handle_.getId(),
            1,
            new ServerRequestCallback<ObjectExplorerInspectionResult>()
            {
               @Override
               public void onResponseReceived(ObjectExplorerInspectionResult result)
               {
                  root_ = result.cast();
                  root_.updateChildOwnership();
                  root_.setExpansionState(ExpansionState.OPEN);
                  synchronize();
               }
               
               @Override
               public void onError(ServerError error)
               {
                  Debug.logError(error);
               }
            });
      
   }
   
   private void synchronize()
   {
      // only include visible data in the table
      // TODO: we should consider how to better handle
      // piecewise updates to the table, rather than
      // shunting a whole new list in
      List<Data> data = flatten(root_, new Filter<Data>()
      {
         @Override
         public boolean accept(Data data)
         {
            return data.isVisible();
         }
      });
      
      Debug.logObject(JsUtil.toJsArray(data));
      
      setData(data);
   }
   
   private List<Data> getData()
   {
      return dataProvider_.getList();
   }
   
   private void setData(List<Data> data)
   {
      dataProvider_.setList(data);
   }
   
   private static final List<Data> flatten(Data data,
                                           Filter<Data> filter) 
   {
      List<Data> list = new ArrayList<Data>();
      flattenImpl(data, filter, list);
      return list;
   }
   
   private static final void flattenImpl(Data data,
                                         Filter<Data> filter,
                                         List<Data> output)
   {
      // exit if we're not accepting data here
      if (filter != null && !filter.accept(data))
         return;
      
      // add data
      output.add(data);
      
      // recurse
      JsArray<Data> children = data.getChildrenData();
      if (children == null)
         return;
      
      for (int i = 0, n = children.length(); i < n; i++)
         flattenImpl(children.get(i), filter, output);
   }
   
   private final Column<Data, Data> nameColumn_;
   private final TextColumn<Data> typeColumn_;
   private final TextColumn<Data> valueColumn_;
   private final TextColumn<Data> sizeColumn_;
   
   private final ListDataProvider<Data> dataProvider_;
   
   private final ObjectExplorerHandle handle_;
   private Data root_;
   
   // Injected ----
   private ObjectExplorerServerOperations server_;
   
   // Static Members ----
   private static final String ACTION_OPEN  = "open";
   private static final String ACTION_CLOSE = "close";
   
   private static final ImageResource2x IMAGE_RIGHT_ARROW =
         new ImageResource2x(ThemeResources.INSTANCE.chevron2x());

   private static final ImageResource2x IMAGE_DOWN_ARROW =
         new ImageResource2x(ThemeResources.INSTANCE.closeChevron2x());

   private static final ImageResource2x IMAGE_TYPE_DATA =
         new ImageResource2x(ThemeResources.INSTANCE.zoomDataset2x());
   
   // Resources, etc ----
   public interface Resources extends RStudioDataGridResources
   {
      @Source({RStudioDataGridStyle.RSTUDIO_DEFAULT_CSS, "ObjectExplorerDataGrid.css"})
      Styles dataGridStyle();
   }
   
   public interface Styles extends RStudioDataGridStyle
   {
   }
   
   private static final Resources RES = GWT.create(Resources.class);
   
   static {
      RES.dataGridStyle().ensureInjected();
   }
   
}
