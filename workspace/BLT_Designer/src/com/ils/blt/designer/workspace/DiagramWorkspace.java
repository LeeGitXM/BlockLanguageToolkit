/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BasicStroke;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.geom.Path2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JPopupMenu;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.editor.PropertyEditorFrame;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.SerializationException;
import com.inductiveautomation.ignition.designer.blockandconnector.AbstractBlockWorkspace;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockActionHandler;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.ConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.ArrowConnectionPainter;
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
 * A Diagram workspace is a tabbed container that occupies the DockManager workspace
 * area. It, in turn, holds DiagnosticsFrames. These are internal frames designed
 * to hold a model diagram. 
 * 
 */
public class DiagramWorkspace extends AbstractBlockWorkspace 
							  implements ResourceWorkspace, DesignableWorkspaceListener {
	private static final String TAG = "DiagramWorkspace";
	private static final long serialVersionUID = 4627016159409031941L;
	public static final String key = "BlockDiagramWorkspace";
	public static final String PREFIX = BLTProperties.BLOCK_PREFIX;
	private final DataFlavor jsonFlavor;
	private final DesignerContext context;
	private final ObjectMapper mapper;
	private final EditActionHandler editActionHandler;
	private Collection<ResourceWorkspaceFrame> frames;
	private ProcessBlockView selectedBlock = null;   // the selection
	protected SaveAction saveAction = null;  // Save properties of a block
	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());

	/**
	 * Constructor 
	 */
	public DiagramWorkspace(DesignerContext ctx) {
		this.context = ctx;
		this.mapper = new ObjectMapper();
		this.editActionHandler = new DiagramActionHandler(this,context);
		this.addDesignableWorkspaceListener(this);
		this.jsonFlavor = new DataFlavor(java.lang.String.class,"BlockList");
		initialize();
	}


	// Initialize the UI
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
	public JPopupMenu getSelectionPopupMenu(List<JComponent> component) {
		if( component.size()>0 ) {
			saveAction = new SaveAction();
		
			JPopupMenu menu = new JPopupMenu();
			menu.add(saveAction);
			menu.addSeparator();
			menu.add(context.getCutAction());
			menu.add(context.getCopyAction());
			menu.add(context.getPasteAction());
			menu.add(context.getDeleteAction());
			return menu;
		}
		return null;
	}
	@Override
	protected ConnectionPainter newConnectionPainter(BlockDiagramModel model) {
		return new DiagramConnectionPainter();
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
	public void onActivation() {
		log.infof("%s: onActivation",TAG);
		
	}


	@Override
	public void onDeactivation() {
		log.infof("%s: onDeactivation",TAG);
	}


	@Override
	public void resetFrames(DockingManager dockManager, DockableBarManager barManager) {
		// TODO Auto-generated method stub
		
	}

	/**
	 * Serialize the supplied blocks and place them on the clipboard. Create a custom MIME
	 * type.
	 * @param blocks the blocks to be saved.
	 */
	@Override
	public String copyBlocks(Collection<Block> blocks) throws SerializationException {
		log.infof("%s: copyBlocks",TAG);
		ObjectMapper mapper = new ObjectMapper();
		StringBuffer json = new StringBuffer("[");
		int index=0;
		for( Block blk:blocks) {
			log.infof("%s: copyBlocks class=%s",TAG,blk.getClass().getName());
			ProcessBlockView view = (ProcessBlockView)blk;
			SerializableBlock sb = ProcessDiagramView.convertBlockViewToSerializable(view);
			if(index>0) json.append(",");
			try{ 
			    json.append(mapper.writeValueAsString(sb));
			}
			catch(JsonProcessingException jpe) {
				log.warnf("%s: Unable to serialize block (%s)",TAG,jpe.getMessage());
			}
			index++;
		}
		
		json.append("]");
		return json.toString();
	}

	/**
	 * Deserialize blocks that were serialized during a "copy" action.
	 * The serialized string was stored on the clipboard.
	 * @return blocks that were recently serialized as a result of a copy.
	 */
	@Override
	public Collection<Block> pasteBlocks(String json) {
		log.infof("%s: pasteBlocks: %s",TAG,json);
		ObjectMapper mapper = new ObjectMapper();
		Collection<Block>results = null;
		JavaType type = mapper.getTypeFactory().constructCollectionType(ArrayList.class, SerializableBlock.class);
		try {
			results = mapper.readValue(json, type);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s: pasteBlocks parse exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s: pasteBlocks mapping exception (%s)",TAG,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s: pasteBlocks IO exception (%s)",TAG,ioe.getLocalizedMessage());
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
		log.infof("%s: open - already open (%s)",TAG,(isOpen(resourceId)?"true":"false"));
		if(isOpen(resourceId) ) {
			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId);
			open(tab);  // Selects?
		}
		else {
			if( context.requestLock(resourceId) ) {
				ProjectResource res = context.getProject().getResource(resourceId);	
				String json = new String(res.getData());
				log.debugf("%s: open - diagram = %s",TAG,json);
				SerializableDiagram sd = null;
				try {
					sd = mapper.readValue(json,SerializableDiagram.class);
					// Synchronize names as the resource may have been re-named since it was serialized
					sd.setName(res.getName());
				} 
				catch (JsonParseException jpe) {
					log.warnf("%s: open parse exception (%s)",TAG,jpe.getLocalizedMessage());
				} 
				catch (JsonMappingException jme) {
					log.warnf("%s: open mapping exception (%s)",TAG,jme.getLocalizedMessage());
				} 
				catch (IOException ioe) {
					log.warnf("%s: open io exception (%s)",TAG,ioe.getLocalizedMessage());
				}
				ProcessDiagramView diagram = ProcessDiagramView.createDiagramView(res.getResourceId(),sd);
				super.open(diagram);
			}
		}
	}
	
	public void close (long resourceId) {
		log.infof("%s: close resource %d",TAG,resourceId);
		super.close(findDesignableContainer(resourceId));
	}
	
	@Override
	protected void onClose(DesignableContainer container) {
		log.infof("%s: onClose",TAG);
		saveDiagram((BlockDesignableContainer)container);
		context.releaseLock(container.getResourceId());
	}
	
	public void saveOpenDiagrams() {
		log.infof("%s: saveOpenDiagrams",TAG);
		for(DesignableContainer dc:openContainers.keySet()) {
			saveDiagram((BlockDesignableContainer)dc);
		}
	}
	
	private void saveDiagram(BlockDesignableContainer c) {
		log.infof("%s: saveDiagram - serializing ...",TAG);
		ProcessDiagramView diagram = (ProcessDiagramView)c.getModel();
		SerializableDiagram sd = diagram.createSerializableRepresentation();
		byte[] bytes = null;
		long resid = c.getResourceId();
		try {
			bytes = mapper.writeValueAsBytes(sd);
			//log.tracef("%s: saveDiagram JSON = %s",TAG,new String(bytes));
			context.updateResource(resid, bytes);
		} 
		catch (JsonProcessingException jpe) {
			log.warnf("%s: saveDiagram processing exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		
		context.updateLock(resid);
	}
	
	
	// =========================== DesignableWorkspaceListener ===========================
	@Override
	public void containerClosed(DesignableContainer container) {
	}
	@Override
	public void containerOpened(DesignableContainer container) {	
	}
	@Override
	public void containerSelected(DesignableContainer container) {
	}
	@Override
	public void itemSelectionChanged(List<JComponent> selections) {
		if( selections!=null && selections.size()==1 ) {
			JComponent selection = selections.get(0);
			log.debugf("%s.DiagramActionHandler: selected a %s",TAG,selection.getClass().getName());
			if( selection instanceof BlockComponent ) {
				BlockComponent bc = ( BlockComponent)selection;
				selectedBlock = (ProcessBlockView)bc.getBlock();
			}
			else {
				selectedBlock = null;
			}
		}
		else {
			log.infof("%s: DiagramActionHandler: deselected",TAG);
			selectedBlock = null;
		}
	}
	
	/**
	 * Paint connections. The cross-section is dependent on the connection type.
	 */
	private class DiagramConnectionPainter extends ArrowConnectionPainter {
		private final Stroke centerlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_CENTERLINE);
		private final Stroke signalOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_SIGNAL);
		private final Stroke truthvalueCoreStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE-2);
		private final Stroke truthvalueOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE);
		private final Stroke dataCoreStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_DATA-2);
		private final Stroke dataOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_DATA);
		private final Stroke informationCoreStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_INFORMATION-2);
		private final Stroke informationOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_INFORMATION);

		/**
		 * Determine the connection class from the origin, then paint accordingly. 
		 * The origin is a BasicAnchorPoint.
		 */
		@Override
		public void paintConnection(Graphics2D g, Connection cxn,Path2D route, boolean selected, boolean hover) {
			ConnectionType ctype = ((BasicAnchorPoint)cxn.getOrigin()).getConnectionType();
			super.hoverColor = WorkspaceConstants.CONNECTION_HOVER;
			super.selectedColor = WorkspaceConstants.CONNECTION_SELECTED;
			if( ctype==ConnectionType.DATA ) {
				super.stroke = dataOutlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_BACKGROUND;
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = dataCoreStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_FILL_DATA;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			else if( ctype==ConnectionType.TRUTHVALUE ) {
				super.stroke = truthvalueOutlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_BACKGROUND;
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = truthvalueCoreStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_FILL_TRUTHVALUE;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			else if( ctype==ConnectionType.INFORMATION ) {
				super.stroke = informationOutlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_BACKGROUND;
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = informationCoreStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_FILL_INFORMATION;
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = centerlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_BACKGROUND;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			else {    // Signal or ANY
				super.stroke = signalOutlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_FILL_SIGNAL;
				super.paintConnection(g, cxn, route, selected, hover);
			}
		}
	}
	
	/**
	 * Edit action handler
	 */
	private class DiagramActionHandler extends BlockActionHandler {
		public DiagramActionHandler(DiagramWorkspace workspace,DesignerContext context) {
			super(workspace,context);
		}
		@Override
		public boolean canCopy() {
			return selectedBlock!=null;
		}


		@Override
		public boolean canDelete() {
			return selectedBlock!=null;
		}

		/**
		 * The data that we've put on the clipboard is a serialized list
		 * of blocks (a JSON String)
		 */
		@Override
		public boolean canPaste(Clipboard clipboard) {
			boolean result = false;
			try{
				result = clipboard.isDataFlavorAvailable(jsonFlavor);
			}
			catch (IllegalStateException ignored) {
			}

			return result;
		}


		@Override
		public Transferable doCopy() {
			log.infof("%s: doCopy",TAG);
			return null;
		}


		@Override
		public Transferable doCut() {
			log.infof("%s: doCut",TAG);
			return null;
		}


		@Override
		public void doDelete() {
			log.infof("%s: doDelete",TAG);
			if( selectedBlock!=null ) getActiveDiagram().deleteBlock(selectedBlock);
			selectedBlock = null;
		}


		@Override
		public void doPaste(Transferable arg) {
			log.infof("%s: doPaste",TAG);
			String json = null;
			try {
				json = (String)arg.getTransferData(DataFlavor.stringFlavor);
				json = new String(json.getBytes("UTF-8"), "UTF-8");  // Convert to UTF-8
				SerializableBlock sb = deserialize(json);
				// Convert to ProcessBlockView
				// Then what ? 
			}
			catch (UnsupportedFlavorException ufe) {
			}
			catch (IOException ioe) {
				ErrorUtil.showError(BundleUtil.get().getStringLenient("DiagramWorkspace.handler.doPaste Error retrieving clipboard (%s)",ioe.getMessage()));
				return;
			}
		}
			
		private SerializableBlock deserialize(String json) {
			SerializableBlock block = null;
			try{
				ObjectMapper mapper = new ObjectMapper();

				block = mapper.readValue(json, SerializableBlock.class);
			}
			// Print stack trace
			catch( Exception ex) {
				log.warnf("%s: deserialize: exception (%s)",TAG,ex.getLocalizedMessage(),ex);
			}
			return block;

		}

		

	}
	
	/**
	 * "Save" implies a push of the block attributes into the model running in the Gateway.
	 */
	private class SaveAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public SaveAction()  {
			super(PREFIX+".SaveBlock",IconUtil.getIcon("window_play"));  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			if( selectedBlock!=null ) {
				
			}
		}
	}
}
