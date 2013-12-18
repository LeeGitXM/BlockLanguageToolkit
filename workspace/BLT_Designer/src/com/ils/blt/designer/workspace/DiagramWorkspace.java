/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JPopupMenu;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.SerializationException;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.DeserializationContext;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.XMLDeserializer;
import com.inductiveautomation.ignition.common.xmlserialization.serialization.XMLSerializer;
import com.inductiveautomation.ignition.designer.blockandconnector.AbstractBlockWorkspace;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.ConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.ArrowConnectionPainter;
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
 * A Diagnostics workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DiagnosticsFrames. These are internal frames designed
 * to hold a model diagram. 
 * 
 */
public class DiagramWorkspace extends AbstractBlockWorkspace implements ResourceWorkspace{
	private static final String TAG = "DiagramWorkspace";
	private static final long serialVersionUID = 4627016159409031941L;
	public static final String key = "BlockDiagramWorkspace";
	public static final String PREFIX = BLTProperties.BLOCK_PREFIX;
	private final DesignerContext context;
	private Collection<ResourceWorkspaceFrame> frames;

	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());

	/**
	 * Constructor 
	 */
	public DiagramWorkspace(DesignerContext ctx) {
		this.context = ctx;
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
		pef.setTitle("Title");
		pef.setTabTitle("Title");
		pef.setSideTitle("Title");
		pef.putClientProperty("menu.text", "Diagram Property Editor");
		frames.add(pef);
	}
	
	
	@Override
	public EditActionHandler getEditActionHandler() {
		// TODO Auto-generated method stub
		return null;
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
			JPopupMenu menu = new JPopupMenu();
			menu.add(context.getCutAction());
			menu.add(context.getCopyAction());
			menu.add(context.getPasteAction());
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
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public List<CommandBar> getToolbars() {
		// TODO list of toolbars to add
		return null;
	}


	@Override
	public JComponent getWorkspace() {
		return this;
	}


	@Override
	public void onActivation() {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void onDeactivation() {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void resetFrames(DockingManager dockManager, DockableBarManager barManager) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public String copyBlocks(Collection<Block> blocks) throws SerializationException {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public Collection<Block> pasteBlocks(String blocks) {
		// TODO Auto-generated method stub
		return null;
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

	public ProcessDiagramView getActiveDiagram() {
		return (ProcessDiagramView)(getSelectedContainer().getModel());
	}
	
	public void open (long resourceId) {
		if(isOpen(resourceId) ) {
			open(findDesignableContainer(resourceId));
		}
		else {
			if( context.requestLock(resourceId) ) {
				ProjectResource res = context.getProject().getResource(resourceId);
				XMLDeserializer des = context.createDeserializer();
				try {
					DeserializationContext desctx =  des.deserialize(res.getData());
					SerializableDiagram sd = (SerializableDiagram)desctx.getRootObjects().get(0);
					ProcessDiagramView diagram = ProcessDiagramView.createDiagramView(res.getResourceId(),sd);
					super.open(diagram);
				}
				catch(SerializationException se) {
					ErrorUtil.showError(se);
				}
			}
		}
	}
	
	public void close (long resourceId) {
		super.close(findDesignableContainer(resourceId));
	}
	
	@Override
	protected void onClose(DesignableContainer container) {
		saveDiagram((BlockDesignableContainer)container);
		context.releaseLock(container.getResourceId());
	}
	
	public void saveOpenDiagrams() {
		for(DesignableContainer dc:openContainers.keySet()) {
			saveDiagram((BlockDesignableContainer)dc);
		}
	}
	
	private void saveDiagram(BlockDesignableContainer c) {
		ProcessDiagramView diagram = (ProcessDiagramView)c.getModel();
		SerializableDiagram sd = diagram.createSerializableRepresentation();
		XMLSerializer ser = context.createSerializer();
		ser.addObject(sd);

		try {
			byte[] bytes = ser.serializeBinary(false);
			long resid = c.getResourceId();
			context.updateResource(resid, bytes);
			context.updateLock(resid);
		} 
		catch (SerializationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * 
	 * @author chuckc
	 *
	 */
	private class DiagramConnectionPainter extends ArrowConnectionPainter {
		Stroke thin = new BasicStroke(2);
		Stroke thick = new BasicStroke(4);
		@Override
		public void paintConnection(Graphics2D g, Connection cxn,
				Path2D route, boolean selected, boolean hover) {
			super.stroke = thick;
			super.standardColor=Color.black;
			super.paintConnection(g, cxn, route, selected, hover);
			super.stroke = thin;
			super.standardColor=Color.cyan;
			super.paintConnection(g, cxn, route, selected, hover);
		}
	}
}
