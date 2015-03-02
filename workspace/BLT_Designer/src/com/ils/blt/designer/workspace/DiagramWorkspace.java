/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Stroke;
import java.awt.datatransfer.DataFlavor;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Path2D;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.editor.PropertyEditorFrame;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.util.LocalObjectTransferable;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.config.ObservablePropertySet;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.SerializationException;
import com.inductiveautomation.ignition.designer.blockandconnector.AbstractBlockWorkspace;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockActionHandler;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.ConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.ArrowConnectionPainter;
import com.inductiveautomation.ignition.designer.designable.DesignPanel;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceListener;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.EditActionHandler;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspace;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.jidesoft.action.CommandBar;
import com.jidesoft.action.DockableBarManager;
import com.jidesoft.docking.DockContext;
import com.jidesoft.docking.DockingManager;

/**
 * A Diagram workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DockableFrames. These are JIDE components. In addition
 * to a palette and editor, the workspace holds a tabbed panel for holding
 * BlockDesignableContainers (DiagramWorkspaces). These contain the visual 
 * representations of the diagrams.
 */
public class DiagramWorkspace extends AbstractBlockWorkspace 
							  implements ResourceWorkspace, DesignableWorkspaceListener,
							  			ChangeListener                                   {
	private static final String TAG = "DiagramWorkspace";
	private static final long serialVersionUID = 4627016159409031941L;
	private static final DataFlavor BlockDataFlavor = LocalObjectTransferable.flavorForClass(ObservablePropertySet.class);
	public static final String key = "BlockDiagramWorkspace";
	public static final String PREFIX = BLTProperties.BLOCK_PREFIX;
	private final ApplicationRequestHandler handler = new ApplicationRequestHandler();
	private final DesignerContext context;
	private final EditActionHandler editActionHandler;
	private final ExecutionManager executionEngine;
	private final NodeStatusManager statusManager;
	private Collection<ResourceWorkspaceFrame> frames;
	protected SaveAction saveAction = null;  // Save properties of a block
	private LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	private PopupListener rightClickHandler;
	private JPopupMenu zoomPopup;

	/**
	 * Constructor:
	 */
	public DiagramWorkspace(DesignerContext ctx) {
		this.context = ctx;
		this.editActionHandler = new BlockActionHandler(this,context);
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		this.addDesignableWorkspaceListener(this);
		this.zoomPopup = createZoomPopup();
		this.rightClickHandler = new PopupListener();
		this.addMouseListener(rightClickHandler);
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		initialize();
		setBackground(Color.red);
	}


	// Initialize the workspace frames.
	private void initialize() {
		// Create palette
		ProcessBlockPalette tabbedPalette = new ProcessBlockPalette(context, this);
		tabbedPalette.setInitMode(DockContext.STATE_FRAMEDOCKED);
		tabbedPalette.setInitSide(DockContext.DOCK_SIDE_NORTH);
		tabbedPalette.setInitIndex(0);
		tabbedPalette.setTitle(BundleUtil.get().getString(PREFIX+".Palette.Title"));
		tabbedPalette.setTabTitle(PREFIX+".Palette.Tab.Title");
		tabbedPalette.setSideTitle("SideTitle");
		tabbedPalette.putClientProperty("menu.text", PREFIX+".Palette.Title");
		
		frames = new ArrayList<ResourceWorkspaceFrame>();
		frames.add(tabbedPalette);
		
		PropertyEditorFrame pef = new PropertyEditorFrame(context, this);
		pef.setInitMode(DockContext.STATE_FRAMEDOCKED);
		pef.setInitSide(DockContext.DOCK_SIDE_WEST);
		pef.setInitIndex(10);
		pef.putClientProperty("menu.text", "Diagram Property Editor");
		frames.add(pef);
	}

	@Override
	public EditActionHandler getEditActionHandler() {
		return editActionHandler;
	}
	
	@Override
	public Collection<ResourceWorkspaceFrame> getFrames() {
		return frames;
	}


	@Override
	public String getKey() {
		return key;
	}

	/**
	 * For popup menu
	 */
	@Override
	public JPopupMenu getSelectionPopupMenu(List<JComponent> selections) {
		if( selections.size()>0 ) {
			JComponent selection = selections.get(0);
			logger.debugf("%s.getSelectionPopupMenu: Component is: %s",TAG,selections.get(0).getClass().getName());
			if( selection instanceof BlockComponent ) {
				
				JPopupMenu menu = new JPopupMenu();
				
				ProcessBlockView pbv = (ProcessBlockView)((BlockComponent)selection).getBlock();
				saveAction = new SaveAction(pbv); 
				// As long as the block is dirty, we can save it. 
				// NOTE: There is always a corresponding block in the gateway. One is created when we drop block from the palette.
				saveAction.setEnabled(pbv.isDirty());
				menu.add(saveAction);
				
				if( selection instanceof BlockComponent && pbv.isCtypeEditable() ) {
					
					// Types are: ANY, DATA, TEXT, TRUTH-VALUE
					// Assume the type from the terminus anchor
					Iterator<ProcessAnchorDescriptor> iterator = pbv.getAnchors().iterator();
					ProcessAnchorDescriptor anch = iterator.next();  // Assumes at least one anchor
					while( anch.getType().equals(AnchorType.Origin) && iterator.hasNext()  ) {
						anch = iterator.next();
					}
					if( anch!=null ) {
						ConnectionType ct = anch.getConnectionType();
						logger.debugf("%s.getSelectionPopupMenu: Connection type is: %s",TAG,ct.name());
						ProcessDiagramView pdv = getActiveDiagram();
						ChangeConnectionAction ccaAny = new ChangeConnectionAction(pdv,pbv,ConnectionType.ANY);
						ccaAny.setEnabled(!ct.equals(ConnectionType.ANY));
						ChangeConnectionAction ccaData = new ChangeConnectionAction(pdv,pbv,ConnectionType.DATA);
						ccaData.setEnabled(!ct.equals(ConnectionType.DATA));
						ChangeConnectionAction ccaText = new ChangeConnectionAction(pdv,pbv,ConnectionType.TEXT);
						ccaText.setEnabled(!ct.equals(ConnectionType.TEXT));
						ChangeConnectionAction ccaTruthvalue = new ChangeConnectionAction(pdv,pbv,ConnectionType.TRUTHVALUE);
						ccaTruthvalue.setEnabled(!ct.equals(ConnectionType.TRUTHVALUE));
					
						JMenu changeTypeMenu = new JMenu(BundleUtil.get().getString(PREFIX+".ChangeConnection"));
						changeTypeMenu.add(ccaAny);
						changeTypeMenu.add(ccaData);
						changeTypeMenu.add(ccaText);
						changeTypeMenu.add(ccaTruthvalue);
						menu.add(changeTypeMenu);
					}
				}
				
				logger.debugf("%s.getSelectionPopupMenu: Selection editor class = %s",TAG,pbv.getEditorClass());
				if( selection instanceof BlockComponent && pbv.getEditorClass() !=null && pbv.getEditorClass().length()>0 ) {
					CustomEditAction cea = new CustomEditAction(pbv);
					menu.add(cea);
				}
				if(pbv.isSignalAnchorDisplayed()) {
					HideSignalAction hsa = new HideSignalAction(this,getActiveDiagram(),pbv);
					menu.add(hsa);
				}
				else {
					ShowSignalAction ssa = new ShowSignalAction(this,getActiveDiagram(),pbv);
					menu.add(ssa);
				}
				ViewInternalsAction via = new ViewInternalsAction(getActiveDiagram(),pbv);
				menu.add(via);
				menu.addSeparator();
				EvaluateAction ea = new EvaluateAction(pbv);
				menu.add(ea);
				ForceAction fa = new ForceAction(getActiveDiagram(),pbv);
				menu.add(fa);
				ResetAction ra = new ResetAction(pbv);
				menu.add(ra);
				if(pbv.isLocked()) {
					UnlockAction ula = new UnlockAction(this,getActiveDiagram(),pbv);
					menu.add(ula);
				}
				else {
					LockAction la = new LockAction(this,getActiveDiagram(),pbv);
					menu.add(la);
				}
				menu.addSeparator();
				menu.add(context.getCutAction());
				menu.add(context.getCopyAction());
				menu.add(context.getPasteAction());
				menu.add(context.getDeleteAction());
				return menu;
			}
			else if( DiagramWorkspace.this.getSelectedContainer().getSelectedConnection()!=null ) {
				JPopupMenu menu = new JPopupMenu();
				menu.add(context.getDeleteAction());    // A connection
				return menu;
			}
		}
		return null;
	}
	@Override
	protected ConnectionPainter newConnectionPainter(BlockDiagramModel bdm) {
		return new DiagramConnectionPainter();
	}
	
	@Override
	protected BlockDesignableContainer newDesignableContainer(BlockDiagramModel bdm) {
		return new DiagramContainer(this,bdm,this.newEdgeRouter(bdm),this.newConnectionPainter(bdm));
	}
	
	@Override
	public MenuBarMerge getMenu() {
		return null;
	}

	// List of toolbars to add
	@Override
	public List<CommandBar> getToolbars() {
		return null;
	}


	@Override
	public JComponent getWorkspace() {
		return this;
	}
	
	@Override
	public int getAcceptableDropActions(DropTargetDragEvent event) {
		return DnDConstants.ACTION_COPY;
	}
	@Override
	public JComponent findDropTarget(List<JComponent> itemsUnderDrop,DropTargetDragEvent event) {
		return this;
	}
	@Override
	public boolean handleDrop(Object droppedOn,DropTargetDropEvent event) {
		if (event.isDataFlavorSupported(BlockDataFlavor)) {
			try {
				if( event.getTransferable().getTransferData(BlockDataFlavor) instanceof ProcessBlockView) {
					ProcessBlockView block = (ProcessBlockView)event.getTransferable().getTransferData(BlockDataFlavor);
					DesignPanel panel = getSelectedDesignPanel();
					BlockDesignableContainer bdc = (BlockDesignableContainer) panel.getDesignable();
					Point dropPoint = SwingUtilities.convertPoint(
							event.getDropTargetContext().getComponent(),
							panel.unzoom(event.getLocation()), bdc);
 
					block.setLocation(dropPoint);
					this.getActiveDiagram().addBlock(block);
					// Null doesn't work here ...
					this.setCurrentTool(getSelectionTool());   // So the next select on workspace does not result in another block
					logger.infof("%s.handleDrop: dropped %s",TAG,event.getTransferable().getTransferData(BlockDataFlavor).getClass().getName());
				}
				else {
					logger.infof("%s.handlerDrop: Unexpected class (%s),  rejected",
							event.getTransferable().getTransferData(BlockDataFlavor).getClass().getName());
				}
				

			} 
			catch (Exception e) {
				ErrorUtil.showError(TAG+" Exception handling drop",e);
			}
		}
		return false;
	}

	@Override
	public void onActivation() {
		logger.infof("%s: onActivation",TAG);
		
	}


	@Override
	public void onDeactivation() {
		logger.infof("%s: onDeactivation",TAG);
	}


	@Override
	public void resetFrames(DockingManager dockManager, DockableBarManager barManager) {
		// TODO Auto-generated method stub
		
	}

	/**
	 * Serialize the supplied blocks and place them on the clipboard. 
	 * @param blocks the blocks to be saved.
	 */
	@Override
	public String copyBlocks(Collection<Block> blocks) throws SerializationException {
		logger.infof("%s: copyBlocks",TAG);
		ObjectMapper mapper = new ObjectMapper();
		String json = null;
		List<SerializableBlock> list = new ArrayList<SerializableBlock>();
		for( Block blk:blocks) {
			logger.infof("%s: copyBlocks class=%s",TAG,blk.getClass().getName());
			ProcessBlockView view = (ProcessBlockView)blk;
			SerializableBlock sb = view.convertToSerializable();
			list.add(sb);
		}
		try{ 
			   json = mapper.writeValueAsString(list);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize block list (%s)",TAG,jpe.getMessage());
		}
		return json;
	}

	/**
	 * Deserialize blocks that were serialized during a "copy" action.
	 * The serialized string was stored on the clipboard.
	 * @return blocks that were recently serialized as a result of a copy.
	 */
	@Override
	public Collection<Block> pasteBlocks(String json) {
		logger.infof("%s.pasteBlocks: %s",TAG,json);
		ObjectMapper mapper = new ObjectMapper();
		Collection<Block>results = new ArrayList<Block>();
		JavaType type = mapper.getTypeFactory().constructCollectionType(ArrayList.class, SerializableBlock.class);
		try {
			List<SerializableBlock>list = mapper.readValue(json, type);
			for(SerializableBlock sb:list) {
				ProcessBlockView pbv = new ProcessBlockView(sb);
				pbv.createPseudoRandomName();
				pbv.createRandomId();
				results.add(pbv);
				// Special handling for an encapsulation block - create its sub-workspace
				if(pbv.isEncapsulation()) {
					try {
						final long newId = context.newResourceId();
						SerializableDiagram diagram = new SerializableDiagram();
						diagram.setName(pbv.getName());
						diagram.setResourceId(newId);
						diagram.setId(UUID.randomUUID());
						diagram.setEncapsulationBlockId(pbv.getId());
						diagram.setDirty(false);    // Will become dirty as soon as we add a block
						logger.infof("%s: new diagram for encapsulation block ...",TAG);
						try{ 
						    json = mapper.writeValueAsString(diagram);
						}
						catch(JsonProcessingException jpe) {
							logger.warnf("%s: Unable to serialize diagram (%s)",TAG,jpe.getMessage());
						}
						logger.infof("%s: serializeDiagram created json ... %s",TAG,json);

						byte[] bytes = json.getBytes();
						logger.debugf("%s: DiagramAction. create new %s resource %d (%d bytes)",TAG,BLTProperties.DIAGRAM_RESOURCE_TYPE,
								newId,bytes.length);
						ProjectResource resource = new ProjectResource(newId,
								BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
								pbv.getName(), ApplicationScope.GATEWAY, bytes);
						resource.setParentUuid(getActiveDiagram().getId());
						executionEngine.executeOnce(new ResourceUpdateManager(this,resource));					
					} 
					catch (Exception err) {
						ErrorUtil.showError(TAG+" Exception pasting blocks",err);
					}
				}
			}
		} 
		catch (JsonParseException jpe) {
			logger.warnf("%s: pasteBlocks parse exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			logger.warnf("%s: pasteBlocks mapping exception (%s)",TAG,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			logger.warnf("%s: pasteBlocks IO exception (%s)",TAG,ioe.getLocalizedMessage());
		}; 
		return results;
	}


	@Override
	protected String getTabToolTip(DesignableContainer ttt) {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	protected void setStatusMessage(String msg) {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @return the currently active diagram, else null
	 */
	public ProcessDiagramView getActiveDiagram() {
		ProcessDiagramView view =null;
		if(getSelectedContainer()!=null ) {
			view = (ProcessDiagramView)(getSelectedContainer().getModel());
		}
		return view; 
	}
	
	public void open (long resourceId) {
		logger.infof("%s: open - already open (%s)",TAG,(isOpen(resourceId)?"true":"false"));
		if(isOpen(resourceId) ) {
			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId);
			open(tab);  // Selects?
		}
		// NOTE: We tried obtaining a lock at this point, but it always seened to be busy without
		//       an explicit save on opening.
		else {
			ProjectResource res = context.getProject().getResource(resourceId);	
			String json = new String(res.getData());
			logger.debugf("%s: open - diagram = %s",TAG,json);
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
				// Synchronize names as the resource may have been re-named since it was serialized
				sd.setName(res.getName());
			} 
			catch (JsonParseException jpe) {
				logger.warnf("%s: open parse exception (%s)",TAG,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				logger.warnf("%s: open mapping exception (%s)",TAG,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				logger.warnf("%s: open io exception (%s)",TAG,ioe.getLocalizedMessage());
			}
			ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
			for( Block blk:diagram.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				diagram.initBlockProperties(pbv);
			}
			super.open(diagram);
			diagram.setDirty(false);  // Newly opened from a serialized resource, should be in-sync.

			// In the probable case that the designer is opened after the diagram has started
			// running in the gateway, obtain any updates
			diagram.registerChangeListeners();
			
			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId);
			tab.setBackground(diagram.getBackgroundColorForState());
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	public void close (long resourceId) {
		logger.infof("%s: close resource %d",TAG,resourceId);
		super.close(findDesignableContainer(resourceId));
	}
	
	// On close we can save the container, no questions asked with a
	// call to: saveDiagram((BlockDesignableContainer)container);
	// As it is ... a dialog pops up.
	@Override
	protected void onClose(DesignableContainer c) {
		logger.debugf("%s: onClose",TAG);
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
		if( diagram.isDirty()  ) {
			Object[] options = {BundleUtil.get().getString(PREFIX+".CloseDiagram.Save"),BundleUtil.get().getString(PREFIX+".CloseDiagram.Revert")};
			int n = JOptionPane.showOptionDialog(null,
					BundleUtil.get().getString(PREFIX+".CloseDiagram.Question"),
					String.format(BundleUtil.get().getString(PREFIX+".CloseDiagram.Title"), diagram.getName()),
					JOptionPane.YES_NO_OPTION,
					JOptionPane.QUESTION_MESSAGE,
					null,         // icon
					options,      // titles of buttons
					options[0]);  //default button title
			if( n==0 ) {
				// Yes, save -- need to serialize the diagram
				SerializableDiagram sd = diagram.createSerializableRepresentation();
				ObjectMapper mapper = new ObjectMapper();
				try {
					byte[] bytes = mapper.writeValueAsBytes(sd);
					context.updateResource(diagram.getResourceId(), bytes);
					saveDiagramResource(container);
				}
				catch(JsonProcessingException jpe) {
					logger.warnf("%s.onClose: serialization exception (%s)",TAG, jpe.getLocalizedMessage());
				}
			}
			else {
				// Mark diagram as clean, since we reverted changes
				diagram.setDirty(false);
				statusManager.clearDirtyChildCount(diagram.getResourceId());
				//context.releaseLock(container.getResourceId());
			}
		}
		diagram.unregisterChangeListeners();
		
	}
	/**
	 * This is called as a result of a user "Save" selection on
	 * the main menu. We actually save all the diagrams.
	 */
	public void saveOpenDiagrams() {
		logger.debugf("%s: saveOpenDiagrams",TAG);
		for(DesignableContainer dc:openContainers.keySet()) {
			saveDiagramResource((BlockDesignableContainer)dc);
		}
	}
	
	/**
	 * This method obtains the project resourceId from the container and then 
	 * saves the project resource. We assume that the resource has been updated.
	 * @param c the tab
	 */
	public void saveDiagramResource(BlockDesignableContainer c) {
		ProcessDiagramView diagram = (ProcessDiagramView)c.getModel();
		logger.debugf("%s.saveDiagramResource - %s ...",TAG,diagram.getDiagramName());
		diagram.setDirty(false);
		long resid = diagram.getResourceId();
		executionEngine.executeOnce(new ResourceUpdateManager(this,context.getProject().getResource(resid)));
		c.setBackground(diagram.getBackgroundColorForState());
		SwingUtilities.invokeLater(new WorkspaceRepainter());
	}
	
	/**
	 * We've made a major change on the currently active diagram. Set its background accordingly.
	 * The diagram should have set its own state.
	 */
	private void updatBackgroundForDirty() {
		BlockDesignableContainer container = getSelectedContainer();
		if( container!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)(container.getModel());
			container.setBackground(view.getBackgroundColorForState());
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	// =========================== DesignableWorkspaceListener ===========================
	@Override
	public void containerClosed(DesignableContainer c) {
		logger.debugf("%s.containerClosed: %s",TAG,c.getName());
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView view = (ProcessDiagramView)(container.getModel());
		view.removeChangeListener(this);
	}
	/**
	 * Container layout manager is:
	 * com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer$BlockLayout
	 */
	@Override
	public void containerOpened(DesignableContainer c) {
		logger.debugf("%s.containerOpened: %s",TAG,c.getName());
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView view = (ProcessDiagramView)(container.getModel());
		view.addChangeListener(this);
	}
	@Override
	public void containerSelected(DesignableContainer container) {
		if( container==null ) logger.infof("%s.containerSelected is null",TAG);
		else logger.debugf("%s.containerSelected: %s",TAG,container.getName());
	}
	@Override
	public void itemSelectionChanged(List<JComponent> selections) {
		if( selections!=null && selections.size()==1 ) {
			JComponent selection = selections.get(0);
			logger.debugf("%s.itemSelectionChanged: selected a %s",TAG,selection.getClass().getName());
		}
		else {
			logger.debugf("%s: DiagramActionHandler: deselected",TAG);
		}
	}
	// ============================== Change Listener ================================
	/**
	 * If the current diagram changes state, then paint the background accordingly.
	 * 
	 * @param event
	 */
	@Override
	public void stateChanged(ChangeEvent event) {
		updatBackgroundForDirty();
	}
	
	/**
	 * Paint connections. The cross-section is dependent on the connection type.
	 */
	private class DiagramConnectionPainter extends ArrowConnectionPainter {
		private final Stroke centerlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_CENTERLINE);
		/**
		 * Determine the connection class from the origin, then paint accordingly. 
		 * The origin is a BasicAnchorPoint.
		 */
		@Override
		public void paintConnection(Graphics2D g, Connection cxn,Path2D route, boolean selected, boolean hover) {
			BasicAnchorPoint origin = (BasicAnchorPoint)cxn.getOrigin();
			ConnectionType ctype = origin.getConnectionType();
			super.hoverColor = WorkspaceConstants.CONNECTION_HOVER;
			super.selectedColor = WorkspaceConstants.CONNECTION_SELECTED;
			// Signal is different in that it has no fill
			if( ctype==ConnectionType.SIGNAL ) {    
				super.stroke = origin.getOutlineStroke();
				super.standardColor=WorkspaceConstants.CONNECTION_FILL_SIGNAL;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			// Text is different in that it has a centerline
			else if( ctype==ConnectionType.TEXT ) {
				super.stroke = origin.getOutlineStroke();
				super.standardColor=origin.getOutlineColor();
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = origin.getCoreStroke();
				super.standardColor=origin.getCoreColor();
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = centerlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_BACKGROUND;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			else {
				super.stroke = origin.getOutlineStroke();
				super.standardColor= origin.getOutlineColor();
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = origin.getCoreStroke();
				super.standardColor=origin.getCoreColor();
				super.paintConnection(g, cxn, route, selected, hover);
			}	
		}
	}
	
	
	/**
	 * Change the connection type of all anchors on this block.
	 * This action is only applicable to a small number of blocks. 
	 */
	private class ChangeConnectionAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ConnectionType connectionType;
		private final ProcessDiagramView diagram;
		private final ProcessBlockView block;
		public ChangeConnectionAction(ProcessDiagramView dgm,ProcessBlockView blk,ConnectionType ct)  {
			super(PREFIX+".ChangeConnectionAction."+ct.name());
			this.connectionType = ct;
			this.diagram = dgm;
			this.block = blk;
		}
		
		// Change all stubs to the selected type.
		// This does NOT make the block dirty, since the changes are automatically
		// synched with the gateway.
		public void actionPerformed(ActionEvent e) {
			block.changeConnectorType(connectionType);
			List<SerializableAnchor> anchors = new ArrayList<SerializableAnchor>();
			for( AnchorDescriptor anchor:block.getAnchors()) {
				anchors.add(block.convertAnchorToSerializable((ProcessAnchorDescriptor)anchor));
			}
			handler.updateBlockAnchors(diagram.getId(),block.getId(),anchors);
		}
	}
	/**
	 * Post a custom editor for the block. This action is expected to
	 * apply to only a few block types. The action should be invoked only
	 * if an editor class has been specified. 
	 */
	private class CustomEditAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public CustomEditAction(ProcessBlockView blk)  {
			super(PREFIX+".ConfigureProperties");
			this.block = blk;
		}
		
		// Display the custom editor
		public void actionPerformed(final ActionEvent e) {
			// Apparently this only works if the class is in the same package (??)
			try{
				Class<?> clss = Class.forName(block.getEditorClass());
				Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {DesignerContext.class,ProcessDiagramView.class,ProcessBlockView.class});
				ProcessDiagramView pdv = getActiveDiagram();
				final JDialog edtr = (JDialog)ctor.newInstance(context,pdv,block); 
				Object source = e.getSource();
				if( source instanceof Component) {
					edtr.setLocationRelativeTo((Component)source);
				}
				edtr.pack();
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						edtr.setVisible(true);
					}
				}); 
			}
			catch(InvocationTargetException ite ) {
				logger.infof("%s.customEditAction: Invocation failed for %s",TAG,block.getEditorClass()); 
			}
			catch(NoSuchMethodException nsme ) {
				logger.infof("%s.customEditAction %s: Constructor taking diagram and block not found (%s)",TAG,block.getEditorClass(),nsme.getMessage()); 
			}
			catch(ClassNotFoundException cnfe) {
				logger.infof("%s.customEditAction: Custom editor class (%s) not found (%s)",TAG,
						block.getEditorClass(),cnfe.getLocalizedMessage());
			}
			catch( InstantiationException ie ) {
				logger.infof("%s.customEditAction: Error instantiating %s (%s)",TAG,block.getEditorClass(),ie.getLocalizedMessage()); 
			}
			catch( IllegalAccessException iae ) {
				logger.infof("%s.customEditAction: Security exception creating %s (%s)",TAG,block.getEditorClass(),iae.getLocalizedMessage()); 
			}
		}
	}
	/**
	 * Trigger the evaluation method of the currently selected block.
	 */
	private class EvaluateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public EvaluateAction(ProcessBlockView blk)  {
			super(PREFIX+".EvaluateBlock",IconUtil.getIcon("window_play"));  // preferences
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			ProcessDiagramView pdv = getActiveDiagram();
			handler.evaluateBlock(pdv.getId().toString(),block.getId().toString());
		}
	}
	/**
	 * Display a dialog that allows the user to enter a value for
	 * each output -- and then force propagation of that value. 
	 */
	private class ForceAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessDiagramView diagram;
		private final ProcessBlockView block;
		public ForceAction(ProcessDiagramView dia,ProcessBlockView blk)  {
			super(PREFIX+".Force");
			this.diagram = dia;
			this.block = blk;
		}
		
		// Display the internals viewer
		public void actionPerformed(final ActionEvent e) {
			final JDialog viewer = (JDialog)new ForceValueSettingsDialog(context.getFrame(),diagram,block);
			Object source = e.getSource();
			if( source instanceof Component) {
				viewer.setLocationRelativeTo((Component)source);
			}
			viewer.pack();
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					viewer.setVisible(true);
				}
			}); 
		}
	}
	/**
	 * Configure the block to show the generic signal connection stub.
	 */
	private class HideSignalAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		private final ProcessDiagramView diagram;
		private final DiagramWorkspace workspace;
		public HideSignalAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".ShowSignal",IconUtil.getIcon("trafficlight_red"));  // preferences
			this.workspace = wksp;
			this.diagram = diag;
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			block.setSignalAnchorDisplayed(false);
			if( !diagram.isDirty()) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
				if( tab!=null ) workspace.saveDiagramResource(tab);
			}
		}
	}
	/**
	 * Plce the currently selected block in lock mode.
	 */
	private class LockAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		private final ProcessDiagramView diagram;
		private final DiagramWorkspace workspace;
		public LockAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".LockBlock",IconUtil.getIcon("lock"));  // preferences
			this.workspace = wksp;
			this.diagram = diag;
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			block.setLocked(true);
			if( !diagram.isDirty()) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
				if( tab!=null ) workspace.saveDiagramResource(tab);
			}
		}
	}
	/**
	 * Trigger the reset action on the current block.
	 */
	private class ResetAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public ResetAction(ProcessBlockView blk)  {
			super(PREFIX+".ResetBlock",IconUtil.getIcon("refresh"));  // preferences
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			ProcessDiagramView pdv = getActiveDiagram();
			handler.resetBlock(pdv.getId().toString(),block.getId().toString());
		}
	}
	/**
	 * "Save" implies a push of the block attributes into the model running in the Gateway.
	 * This, in turn, makes the parent diagram resource dirty.
	 */
	private class SaveAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public SaveAction(ProcessBlockView blk)  {
			super(PREFIX+".SaveBlock",IconUtil.getIcon("window_play"));  // preferences
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			logger.info("DiagramWorkspace: SAVE BLOCK");
			ProcessDiagramView pdv = getActiveDiagram();
			handler.setBlockProperties(pdv.getId(),block.getId(), block.getProperties());
			block.setDirty(false);
			statusManager.clearDirtyChildCount(pdv.getResourceId());
		}
	}
	/**
	 * Configure the block to show the generic signal connection stub.
	 */
	private class ShowSignalAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		private final ProcessDiagramView diagram;
		private final DiagramWorkspace workspace;
		public ShowSignalAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".ShowSignal",IconUtil.getIcon("trafficlight_green"));  // preferences
			this.workspace = wksp;
			this.diagram = diag;
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			block.setSignalAnchorDisplayed(true);
			if( !diagram.isDirty()) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
				if( tab!=null ) workspace.saveDiagramResource(tab);
			}
		}
	}
	/**
	 * Trigger the evaluation method of the currently selected block.
	 */
	private class UnlockAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		private final ProcessDiagramView diagram;
		private final DiagramWorkspace workspace;
		public UnlockAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".UnlockBlock",IconUtil.getIcon("lock_open"));  // preferences
			this.workspace = wksp;
			this.diagram = diag;
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			block.setLocked(false);
			if( !diagram.isDirty()) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
				if( tab!=null ) workspace.saveDiagramResource(tab);
			}
		}
	}
	/**
	 * Post an internals viewer for the block. The default shows
	 * only name, class and UUID. Blocks may transmit additional
	 * parameters as is useful. 
	 */
	private class ViewInternalsAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessDiagramView diagram;
		private final ProcessBlockView block;
		public ViewInternalsAction(ProcessDiagramView dia,ProcessBlockView blk)  {
			super(PREFIX+".ViewInternals");
			this.diagram = dia;
			this.block = blk;
		}
		
		// Display the internals viewer
		public void actionPerformed(final ActionEvent e) {
			final JDialog viewer = (JDialog)new BlockInternalsViewer(context.getFrame(),diagram,block);
			Object source = e.getSource();
			if( source instanceof Component) {
				viewer.setLocationRelativeTo((Component)source);
			}
			viewer.pack();
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					viewer.setVisible(true);
				}
			}); 

		}
	}
	// ===================================== Right-click for popup on tab =======================================
	private JPopupMenu createZoomPopup() {
        JPopupMenu popup = new JPopupMenu("Zoom");
        popup.add(createMenuItem("25%"));
        popup.add(createMenuItem("50%"));
        popup.add(createMenuItem("75%"));
        popup.add(createMenuItem("100%"));
        popup.add(createMenuItem("200%"));
        return popup;
    }
 
	// For some reason adding an action command, didn't
	// work, but the inner class listener did ...
    private JMenuItem createMenuItem(final String s) {
        JMenuItem item = new JMenuItem(s);
        item.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				int zoom = 100;
	            if(s.equals("25%"))
	                zoom = 25;
	            else if(s.equals("50%"))
	                zoom = 50;
	            else if(s.equals("75%"))
	                zoom = 75;
	            else if(s.equals("200%"))
	                zoom = 200;
	            else
	            	zoom = 100;
	            
	            DiagramWorkspace.this.getSelectedDesignPanel().setZoom(zoom);
	            SwingUtilities.invokeLater(new WorkspaceRepainter());
			}
        	
        });
        return item;
    }
 
  
    private class PopupListener extends MouseAdapter {
        public void mousePressed(MouseEvent e) {
            checkForPopup(e);
        }
        public void mouseReleased(MouseEvent e) {
            checkForPopup(e);
        }
        public void mouseClicked(MouseEvent e) {
            checkForPopup(e);
        }
 
        private void checkForPopup(MouseEvent e)  {
            if(e.isPopupTrigger())
            {
                Component c = e.getComponent();
                zoomPopup.show(c, e.getX(), e.getY());
            }
        }
    }
}
