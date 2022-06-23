package com.ils.blt.designer.workspace;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JComponent;

import com.inductiveautomation.factorypmi.application.FPMISystem;
import com.inductiveautomation.factorypmi.application.FPMIWindow;
import com.inductiveautomation.factorypmi.application.binding.Adapter;
import com.inductiveautomation.factorypmi.application.binding.BindingRoot;
import com.inductiveautomation.factorypmi.application.binding.InteractionDescriptor;
import com.inductiveautomation.factorypmi.application.binding.PropertyAdapter;
import com.inductiveautomation.factorypmi.application.binding.util.BindUtilities;
import com.inductiveautomation.factorypmi.application.components.template.VisionTemplate;
import com.inductiveautomation.factorypmi.application.components.util.FPMILayout;
import com.inductiveautomation.factorypmi.application.model.WindowCache;
import com.inductiveautomation.factorypmi.designer.model.VisionDesignerImpl;
import com.inductiveautomation.factorypmi.designer.workspace.LayoutManipulator;
import com.inductiveautomation.factorypmi.designer.workspace.tools.PasteTool;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.client.util.gui.IgnitionSwingUtilities;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.xmlserialization.SerializationException;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.DeserializationContext;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.XMLDeserializer;
import com.inductiveautomation.ignition.common.xmlserialization.serialization.XMLSerializer;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceAdapter;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceListener;
import com.inductiveautomation.ignition.designer.designable.IDesignTool;
import com.inductiveautomation.ignition.designer.gui.CommonUI;
import com.inductiveautomation.ignition.designer.model.AbstractEditActionHandler;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.EditActionHandler;
import com.inductiveautomation.vision.api.client.components.model.TopLevelContainer;
import com.inductiveautomation.vision.api.client.components.model.VisionContainer;










public class CopyPasteHandler extends AbstractEditActionHandler implements EditActionHandler {
  public static final String ARCHIVE_TYPE_KEY = "fpmi.archive.type";
  public static final String ARCHIVE_TYPE_WINDOW = "windows";
  public static final String ARCHIVE_TYPE_TEMPLATE = "template";
  public static final String ARCHIVE_TYPE_COMPONENTS = "components";
  private DiagramWorkspace workspace;
  
  private boolean pasteLegacyFormat = false;
  private boolean pastePending = false;
  private InputStream pasteData = null;
  private String pasteDataType;
  private DesignerContext context;
  
  /**
   * 
   * @param workspace
   */
  public CopyPasteHandler(DiagramWorkspace workspace) {
    this.workspace = workspace;

    
    workspace.addDesignableWorkspaceListener((DesignableWorkspaceListener)new DesignableWorkspaceAdapter()
        {
          public void itemSelectionChanged(List<JComponent> newSelection) {
            EventQueue.invokeLater(() -> CopyPasteHandler.this.fireChangeEvent());
          }
        });
    workspace.addPropertyChangeListener("designTime", evt -> fireChangeEvent());
  }
  
  @Override
  public boolean canDelete() {
    return (this.workspace.isDesignTime() && this.workspace.getSelectedItemsCount() > 0);
  }
  
  @Override
  public boolean canCopy() {
    boolean b = (this.workspace.isDesignTime() && this.workspace.getSelectedItemsCount() > 0);

    
    return b;
  }
  
  @Override
  public boolean canPaste(Clipboard clipboard) {
    boolean b = false;
    
    try {
      b = (this.workspace.isDesignTime() && clipboard.isDataFlavorAvailable(DataFlavor.stringFlavor));
    } catch (IllegalStateException illegalStateException) {}

    
    //this.workspace.handler.pasteImmediate.setEnabled(b);
    
    return b;
  }

  public boolean canPaste(Transferable data) {
    boolean b = (this.workspace.isDesignTime() && data != null && data.isDataFlavorSupported(DataFlavor.stringFlavor));
    
    //this.workspace.handler.pasteImmediate.setEnabled(b);
    
    return b;
  }
  
  public void startLegacyPaste(String xml, boolean isWindow, boolean interactive) {
    try {
      String archiveType = isWindow ? "windows" : "components";
      startPaste(new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8)), archiveType, interactive);
      this.pasteLegacyFormat = true;
      if (isWindow) {
        doPaste(null, null);
      }
    } catch (Exception ex) {
      ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.ErrorDecodingPasteData", new Object[0]), ex);
    } 
  }



  
  public void startPaste(boolean interactive) {
    if (this.workspace.isDesignTime()) {
      Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
      Transferable t = cb.getContents(this);
      String xml = null;
      try {
        xml = (String)t.getTransferData(DataFlavor.stringFlavor);
        
        xml = new String(xml.getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8);
        startPaste(xml, interactive);
      } catch (UnsupportedFlavorException unsupportedFlavorException) {
      
      } catch (IOException e) {
        ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.ErrorRetrievingClipboardData", new Object[0]), e);
      } 
    } 
  }
  
  public void startPaste(String xml, boolean interactive) {
    try {
      XMLDeserializer headerReader = context.createDeserializer();
      Map<String, String> attrs = headerReader.readRootAttributes(xml);
      String pasteType = attrs.get("fpmi.archive.type");
      if (pasteType != null) {
        startPaste(new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8)), pasteType, interactive);
        if ("windows".equals(pasteType) || "template".equals(pasteType)) {
          doPaste(null, null);
        }
      } else {
        ErrorUtil.showWarning("Unrecognized data in clipboard.");
      } 
    } catch (Exception ex) {
      ErrorUtil.showError("Error decoding pasted data.", ex);
    } 
  }
  
  public void doPaste(VisionContainer dropContainer, Point dropLocation) {
    doPaste(dropContainer, dropLocation, false);
  }
  
  public void doPaste(VisionContainer dropContainer, Point dropLocation, boolean pasteAgain) {
    if (!this.pastePending) {
      return;
    }
    
    BindingRoot root = (BindingRoot)IgnitionSwingUtilities.getAncestorOfClass(BindingRoot.class, (JComponent)dropContainer);
    
    VisionDesignerImpl designer = (VisionDesignerImpl)context;
    DesignerContext context = designer.getDesignerContext();
    
    InputStream data = null;
    if (pasteAgain) {
      try {
        byte[] bytes = pasteData.readAllBytes();
        data = new ByteArrayInputStream(bytes);
        this.pasteData = new ByteArrayInputStream(bytes);
      } catch (IOException e) {
        ErrorUtil.showError(e);
        return;
      } 
    } else {
      setPastePending(false);
      data = this.pasteData;
    } 
    try {
      CommonUI.setWaitCursor(context.getFrame());
      
      boolean itemIsComponents = this.pasteDataType.equals("components");
      
      List<JComponent> components = new ArrayList<>();
      List<?> adapters = null;
      FPMIWindow pastedWindow = null;
      VisionTemplate pastedTemplate = null;
      
      Set<String> dependentPaths = new HashSet<>();
      
      Set<Adapter> badAdapters = new HashSet<>();
      
      try {
        List<Object> objects = deserializeCopiedObjects(root, context, data, itemIsComponents);
        
        if (objects.size() == 0) {
          ErrorUtil.showError(BundleUtil.get()
              .getStringLenient("fpmi.CopyPasteHandler.Errors.NothingToPaste"));
        } else if (objects.size() == 1 && objects.get(0) instanceof FPMIWindow) {
          pastedWindow = (FPMIWindow)objects.get(0);
        } else if (objects.size() == 1 && objects.get(0) instanceof VisionTemplate) {
          pastedTemplate = (VisionTemplate)objects.get(0);
        } else {
          for (Object o : objects) {
            if (o instanceof JComponent) {
              components.add((JComponent)o); continue;
            }  if (o instanceof List) {
              adapters = (List)o;
            }
          } 
        } 
        
        if (itemIsComponents && adapters != null) {
          processAdapters(root, adapters, dependentPaths, badAdapters);
        }
      } catch (SerializationException e) {
        if (e.getCause() instanceof ClassNotFoundException) {
          ClassNotFoundException cnf = (ClassNotFoundException)e.getCause();
          handleClassNotFoundDeserializationException(cnf);
        } else {
          ErrorUtil.showError((Throwable)e);
        } 
      } 
      
      if (pastedWindow != null) {
        designer.addNewWindow(designer.getSelectedWindowFolder(), pastedWindow);
      } else if (pastedTemplate != null) {
        designer.addNewTemplate(designer.getSelectedTemplateFolder(), pastedTemplate);
      } else if (itemIsComponents && components.size() >= 1) {
        
        if (dependentPaths.size() > 0) {
          generateBrokenBindingsWarning(dependentPaths);
        }
        
        Point2D.Double baseLocation = calculateBaseLocation(dropLocation, components);

        
        addAndStartupComponents(dropContainer, components, baseLocation);

        
        addAndStartupAdapters(root, adapters, badAdapters);

        
        addUndoAction(dropContainer, components);

        
        this.workspace.setSelectedItems(components);
        context.getStatusBar().setMessage(
            BundleUtil.i18n("CopyPasteHandler.Messages.StatusSuccess" + ((components.size() > 1) ? ".Plural" : ""), new Object[0]));
      } 
    } finally {
      CommonUI.setDefaultCursor(context.getFrame());
    } 
  }








  
  private void processAdapters(BindingRoot root, List<?> adapters, Set<String> dependentPaths, Set<Adapter> badAdapters) {
    for (Object o : adapters) {
      InteractionDescriptor[] ids = ((Adapter)o).getInteractions();
      if (ids != null && ids.length > 0) {
        for (InteractionDescriptor id : ids) {
          if (id._getSymbolicSourcePath() != null) {
            Component c = root.getComponentForPath(id._getSymbolicSourcePath());
            if (c == null) {
              dependentPaths.add(id._getSymbolicSourcePath());
              badAdapters.add((Adapter)o);
            } else {
              id.setSource(c);
            } 
          } 
        } 
      }
    } 
  }

  
  private List<Object> deserializeCopiedObjects(BindingRoot root, DesignerContext context, InputStream data, boolean itemIsComponents) throws SerializationException {
    List<Object> objects;
    if (!this.pasteLegacyFormat) {
      XMLDeserializer deserializer = context.createDeserializer();
      DeserializationContext deser = deserializer.deserialize(data);
      for (SerializationException ex : deser.getWarnings()) {
        ErrorUtil.showError((Throwable)ex);
      }
      objects = deser.getRootObjects();
    } else {
      objects = WindowCache._legacyDeserialize(data, itemIsComponents ? root : null);
    } 
    return objects;
  }

  
  private void generateBrokenBindingsWarning(Set<String> dependentPaths) {
    StringBuilder errMessage = new StringBuilder(BundleUtil.i18n("fpmi.CopyPasteHandler.Warnings.PasteWarning.UnsatisfiedDependency.Header", new Object[0]));
    for (String path : dependentPaths) {
      errMessage.append("<li><code>").append(path).append("</code></li>");
    }
    errMessage.append("</ul>");
    ErrorUtil.showWarning(errMessage.toString(), BundleUtil.i18n("fpmi.CopyPasteHandler.Warnings.PasteWarning.Title", new Object[0]));
  }
  
  private void addUndoAction(VisionContainer dropContainer, List<JComponent> components) {
    //DeleteOrAddUndoAction undoAction = new DeleteOrAddUndoAction(this.workspace, dropContainer, new ArrayList<>(components), null, "Paste", true);
    
    //UndoManager.getInstance().add(undoAction);
  }
  
  private void addAndStartupAdapters(BindingRoot root, List<?> adapters, Set<Adapter> badAdapters) {
    for (Object adapter : adapters) {
      Adapter pa = (Adapter)adapter;
      if (!badAdapters.contains(pa)) {
        if (pa instanceof PropertyAdapter) {
          BindUtilities.pullCurrentValues((PropertyAdapter)pa);
        }
        root.getInteractionController().addAdapter(pa.getTarget(), pa);
      } 
    } 
  }

  
  private void addAndStartupComponents(VisionContainer dropContainer, List<JComponent> components, Point2D.Double baseLocation) {
    for (int i = components.size() - 1; i >= 0; i--) {
      Component comp = components.get(i);
      if (baseLocation != null)
      {
        LayoutManipulator.setPositionDelta((JComponent)comp, baseLocation.x, baseLocation.y);
      }
      //this.workspace.getLayoutManipulator().add(dropContainer, (JComponent)comp);
    } 
  }






  
  private Point2D.Double calculateBaseLocation(Point dropLocation, List<JComponent> components) {
    Point2D.Double baseLocation = null;
    if (dropLocation != null) {
      baseLocation = new Point2D.Double();
      Point2D.Double compLocation = new Point2D.Double();
      double distance = Double.MAX_VALUE;
      for (Component comp : components) {
        if (getLocation(comp, compLocation).distance(0.0D, 0.0D) < distance) {





          
          distance = compLocation.distance(0.0D, 0.0D);
          baseLocation.x = compLocation.x;
          baseLocation.y = compLocation.y;
        } 
      } 
      
      baseLocation.x = dropLocation.x - baseLocation.x;
      baseLocation.y = dropLocation.y - baseLocation.y;
    } 
    return baseLocation;
  }
  
  private Point2D getLocation(Component comp, Point2D point) {
    Rectangle2D bounds = FPMILayout.getBounds((JComponent)comp);
    point.setLocation(bounds.getX(), bounds.getY());
    return point;
  }













  
  public synchronized void startPaste(InputStream data, String dataType, boolean interactive) {
    if (!"windows".equals(dataType) && 
      !"components".equals(dataType) && 
      !"template".equals(dataType)) {
      throw new IllegalArgumentException(String.format("Paste data-type \"%s\" not recognized.", new Object[] { dataType }));
    }
    setPastePending(true);
    this.pasteData = data;
    this.pasteLegacyFormat = false;
    this.pasteDataType = dataType;
    if (interactive && "components".equals(dataType)) {
      //this.workspace.setCurrentTool((IDesignTool)new PasteTool(this));
    }
  }
  
  private void handleClassNotFoundDeserializationException(ClassNotFoundException cnf) {
    String classname = cnf.getMessage();
    
    String extraMessage = "";
    if (classname.startsWith("com.inductiveautomation.factorypmi.plugins.")) {
      
      String[] pluginNames = classname.split("\\.");
      if (pluginNames.length >= 5) {
        String pluginName = pluginNames[4];
        extraMessage = BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.PasteError.MissingResource.PluginFragment", new Object[] { pluginName });
      } 
    } 


    
    ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.PasteError.MissingResource.Body", new Object[] { extraMessage, classname }));
  }




  
  public void cancelPaste() {
    setPastePending(false);
    this.pasteData = null;
    this.workspace.setCurrentTool(this.workspace.getSelectionTool());
  }



  
  public boolean isPastePending() {
    return this.pastePending;
  }
  
  protected void setPastePending(boolean b) {
    this.pastePending = b;
    //this.workspace.handler.cancelPaste.setEnabled(b);
  }
  
  private boolean canDeleteItem(Component item, TopLevelContainer container) {
    if (container instanceof FPMIWindow) {
      return (((FPMIWindow)container).getRootContainer() != item);
    }
    return true;
  }
  
  @Override
  public void doDelete() {
    if (!this.workspace.isDesignTime()) {
      return;
    }
    
    TopLevelContainer window = null; //this.workspace.getSelectedContainer();
    Component selectedItem = (this.workspace.getSelectedItemsCount() > 0) ? this.workspace.getSelectedItems().get(0) : null;
    
    if (selectedItem != null && window != null && !(selectedItem instanceof TopLevelContainer)) {
      
      if (canDeleteItem(selectedItem, window)) {


        
        List<JComponent> topComps = new ArrayList<>(this.workspace.getSelectedItems());
        if (LayoutManipulator.componentsAreSiblings(topComps)) {



          
          List<JComponent> allComps = getAllAffectedComponents(topComps);
          int size = topComps.size();
          if (Boolean.TRUE.equals(ErrorUtil.showConfirm(
                BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.ConfirmComponentDelete" + (
                  (size > 1) ? ".Multiple" : ""), new Object[] {
                    (size > 1) ? Integer.valueOf(size) : ((JComponent)topComps.get(0)).getName()
                  }), BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.ConfirmComponentDelete.Title", new Object[0])))) {
            
            try {
              checkDeleteConditions(window, allComps);
            } catch (Exception e) {
              ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotDelete", new Object[0]) + BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotDelete", new Object[0]));

              
              return;
            } 
            
            if (shouldDeleteComps(window, allComps))
            {
              //doDeleteImpl(window, topComps, allComps, false, true);
              
              context.getStatusBar().setMessage(
                  BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.DeleteSuccess", new Object[] { Integer.valueOf(topComps.size()) }));
            }
          
          } 
        } else {
          
          ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.NonSiblings", new Object[0]));
        } 
      } else {
        ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotDeleteRootContainer", new Object[0]));
      } 
    } else if (selectedItem instanceof TopLevelContainer) {
      //this.workspace.deleteTopLevelContainer(((TopLevelContainer)selectedItem).getResourcePath(), true);
    } 
  }




  
  boolean shouldDeleteComps(TopLevelContainer parentWindow, List<JComponent> allComps) {
    int numLocked = 0;
    for (JComponent c : allComps) {
      if (Boolean.TRUE.equals(c.getClientProperty("v.lck"))) {
        numLocked++;
      }
    } 
    if (numLocked > 0) {
      return Boolean.TRUE.equals(ErrorUtil.showConfirm(
            BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.ConfirmLockedDelete", new Object[] { Integer.valueOf(numLocked)
              }), BundleUtil.i18n("words.confirm", new Object[0])));
    }
    return true;
  }




  
  public void doDuplicateSelection() {
    if (this.workspace.getSelectedItemsCount() > 0) {
      
      TopLevelContainer topLevelContainer = null;  //this.workspace.getSelectedContainer();
      
      if (topLevelContainer instanceof FPMIWindow && this.workspace
        .getSelectedItems().contains(((FPMIWindow)topLevelContainer).getRootContainer())) {
        String cannotDuplicateMessage = BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotDuplicateRoot", new Object[0]);
        
        ErrorUtil.showError(cannotDuplicateMessage);
        
        return;
      } 
      boolean isWindow = this.workspace.getSelectedItems().get(0) instanceof FPMIWindow;
      if (isWindow) {
        String data = doCopyOpToString(false);
        if (data != null) {
          startPaste(data, false);
          doPaste(null, null);
        } 
      } else {
        VisionContainer dropContainer = (VisionContainer)((JComponent)this.workspace.getSelectedItems().get(0)).getParent();
        String data = doCopyOpToString(false);
        if (data != null) {
          startPaste(data, false);
          doPaste(dropContainer, null);
        } 
      } 
    } 
  }



  
  public void doCopyToClipboard() {
    doCopyOpToClipboard(false);
  }



  
  public void doCutToClipboard() {
    doCopyOpToClipboard(true);
  }




  
  public Transferable doCopy() {
    String s = doCopyOpToString(false);
    if (s != null) {
      return new StringSelection(s);
    }
    return null;
  }





  
  public Transferable doCut() {
    String s = doCopyOpToString(true);
    if (s != null) {
      return new StringSelection(s);
    }
    return null;
  }





  
  private void doCopyOpToClipboard(boolean cut) {
    if (this.workspace.isDesignTime()) {
      String xmlForClipboard = doCopyOpToString(cut);
      if (xmlForClipboard != null) {
        Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
        StringSelection data = new StringSelection(xmlForClipboard);
        cb.setContents(data, data);
      } 
    } 
  }




  
  private String doCopyOpToString(boolean cut) {
    StringWriter out = new StringWriter();
    
    XMLSerializer serializer = context.createSerializer();
    
    CommonUI.setWaitCursor(context.getFrame());
    FPMISystem.setSerializing(true);
    String xmlForClipboard = null;
    
    try {
      List<JComponent> items = this.workspace.getSelectedItems();
      
      if (items.size() == 1 && items.get(0) instanceof TopLevelContainer) {
        String type, noun; JComponent item = items.get(0);

        
        if (item instanceof FPMIWindow) {
          type = "windows";
          noun = BundleUtil.get().getString("fpmi.Window.Noun");
        } else if (item instanceof VisionTemplate) {
          type = "template";
          noun = BundleUtil.get().getString("fpmi.Template.Noun");
        } else {
          throw new IllegalArgumentException(String.format("Unknown type of top-level container: %s", new Object[] { item
                  .getClass() }));
        } 
        serializer.addRootAttribute("fpmi.archive.type", type);
        
        serializer.addObject(items.get(0));
        try {
          serializer.serializeXML(out, "UTF-8");
          if (cut) {
            ResourcePath id = ((TopLevelContainer)items.get(0)).getResourcePath();
            //this.workspace.deleteTopLevelContainer(id, false);
          } 
          
          context.getStatusBar().setMessage(
              cut ? BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.CutWindow", new Object[] { noun
                }) : BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.CopiedWindow", new Object[] { noun }));
        }
        catch (SerializationException ex) {
          ErrorUtil.showError((Throwable)ex);
          return null;
        } 
      } else if (this.workspace.getSelectedContainer() != null && items.size() > 0) {
        TopLevelContainer window = null;  //this.workspace.getSelectedContainer();
        
        boolean isRC = (window instanceof FPMIWindow && items.get(0) == ((FPMIWindow)window).getRootContainer());
        
        if (cut && isRC) {
          ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotCutRoot", new Object[0]));
          String noun = null; return noun;
        }  if (LayoutManipulator.componentsAreSiblings(items) || isRC) {
          List<JComponent> allComps = getAllAffectedComponents(items);
          
          if (cut) {
            
            try {


              
              checkDeleteConditions(window, allComps);
            } catch (Exception e) {
              ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotCut", new Object[0]) + BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.CannotCut", new Object[0]));
              return null;
            } 
          }

          
          LayoutManipulator.validateRoot((Component)window);
          
          List<Adapter> adapters = new ArrayList<>();
          for (Component c : allComps) {
            adapters.addAll(Arrays.asList(window.getInteractionController().getAllAdaptersForTarget(c)));
          }









          
          for (Adapter a : adapters) {
            InteractionDescriptor[] interactions = a.getInteractions();
            
            for (InteractionDescriptor interaction : interactions) {
              if (!allComps.contains(interaction.getSource())) {
                interaction.setSerializeSymbolically(true);
                
                interaction._setSymbolicSourcePath(window.getPathForComponent(interaction
                      .getSource()));
              } 
            } 
          } 

          
          serializer.addRootAttribute("fpmi.archive.type", "components");



          
          serializer.addObject(adapters);




          
          List<JComponent> comps = new ArrayList<>(items);
          for (JComponent o : comps) {
            serializer.addObject(o);
          }
          
          try {
            serializer.serializeXML(out, "UTF-8");
            
            if (cut) {
              //doDeleteImpl(window, comps, allComps, true, true);
            }
            
            context.getStatusBar().setMessage(
                BundleUtil.i18n(cut ? "fpmi.CopyPasteHandler.Messages.ComponentCutSuccess" : 
                  "fpmi.CopyPasteHandler.Messages.ComponentCopySuccess", new Object[] { Integer.valueOf(comps.size()) }));

            
            for (Adapter pa : adapters) {
              InteractionDescriptor[] interactions = pa.getInteractions();
              for (InteractionDescriptor interaction : interactions) {
                interaction.setSerializeSymbolically(false);
                interaction._setSymbolicSourcePath(null);
              } 
            } 
          } catch (SerializationException ex) {
            ErrorUtil.showError((Throwable)ex);
            return null;
          } 
        } else {
          
          ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.NonSiblings", new Object[0]));
          return null;
        } 
      } else {
        ErrorUtil.showError(BundleUtil.i18n("fpmi.CopyPasteHandler.Messages.SelectSiblings", new Object[0]));
        return null;
      } 
      xmlForClipboard = out.toString();
      return xmlForClipboard;
    } finally {
      CommonUI.setDefaultCursor(context.getFrame());
      FPMISystem.setSerializing(false);
      
    } 
  }





  
  static List<JComponent> getAllAffectedComponents(List<JComponent> sibs) {
    List<JComponent> all = new ArrayList<>(sibs);
    for (JComponent sib : sibs) {
      if (sib instanceof VisionContainer) {
        gatherChildren((VisionContainer)sib, all);
      }
    } 
    return all;
  }




  
  private static void gatherChildren(VisionContainer container, List<JComponent> list) {
    for (int i = 0; i < container.getComponentCount(); i++) {
      list.add((JComponent)container.getComponent(i));
      if (container.getComponent(i) instanceof VisionContainer) {
        gatherChildren((VisionContainer)container.getComponent(i), list);
      }
    } 
  }

  
  void checkDeleteConditions(TopLevelContainer parentWindow, List<JComponent> allComps) throws Exception {
    for (int i = 0; i < allComps.size(); i++) {
      JComponent c = allComps.get(i);
      
      InteractionDescriptor[] ids = parentWindow.getInteractionController().getInteractionDescriptorsWithSource(c);
      for (InteractionDescriptor id : ids) {
        if (!allComps.contains(id.getAdapter().getTarget())) {
          
          StringBuilder dependent = new StringBuilder(parentWindow.getPathForComponent(id.getAdapter().getTarget()));
          if (id.getAdapter() instanceof PropertyAdapter) {
            dependent.append(".").append(((PropertyAdapter)id.getAdapter()).getTargetPropertyName());
          }
          String thisComponent = parentWindow.getPathForComponent(c);
          throw new Exception(BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.SelectedComponentDependency", new Object[] { dependent, thisComponent }));
        } 
      } 
    } 
  }
  
  public void doPaste(Transferable t) {
    doPasteImpl(t);
  }
  
  public boolean doPasteImpl(Transferable t) {
    String xml = null;
    try {
      xml = (String)t.getTransferData(DataFlavor.stringFlavor);
      
      xml = new String(xml.getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8);
      startPaste(xml, true);
      return true;
    } catch (UnsupportedFlavorException unsupportedFlavorException) {
    
    } catch (IOException e) {
      ErrorUtil.showError(
          BundleUtil.i18n("fpmi.CopyPasteHandler.Errors.ErrorRetrievingClipboardData", new Object[0]), e);
    } 
    return false;
  }
}
