/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.schematic.workspace;

import java.awt.Graphics2D;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.designer.editor.PropertyEditorFrame;
import com.ils.blt.designer.schematic.BLTSchematicDesignerHook;
import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockPalette;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceConstants;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.ConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.ArrowConnectionPainter;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.jidesoft.docking.DockContext;

/**
 * A Diagram workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DockableFrames. These are JIDE components. In addition
 * to a palette and editor, the workspace holds a tabbed panel for holding
 * BlockDesignableContainers (DiagramWorkspaces). These contain the visual 
 * representations of the diagrams.
 */
public class SchematicDiagramWorkspace extends DiagramWorkspace           {
	private static final long serialVersionUID = 6885838413495575622L;
	private static final String key = "SchematicDiagramWorkspace";
	/**
	 * Constructor:
	 */
	public SchematicDiagramWorkspace(DesignerContext ctx) {
		super(ctx);
		this.statusManager = ((BLTSchematicDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		initialize();
	}


	// Initialize the workspace frames.
	private void initialize() {
		// Create palette
		ProcessBlockPalette tabbedPalette = new ProcessBlockPalette(context, this,handler);
		tabbedPalette.setInitMode(DockContext.STATE_FRAMEDOCKED);
		tabbedPalette.setInitSide(DockContext.DOCK_SIDE_NORTH);
		tabbedPalette.setInitIndex(0);
		tabbedPalette.setTitle(BundleUtil.get().getString(PREFIX+".Palette.Title"));
		tabbedPalette.setTabTitle(PREFIX+".Palette.Tab.Title");
		tabbedPalette.setSideTitle("SideTitle");
		tabbedPalette.putClientProperty("menu.text", PREFIX+".Palette.Title");
		
		frames = new ArrayList<ResourceWorkspaceFrame>();
		frames.add(tabbedPalette);
		
		PropertyEditorFrame pef = new PropertyEditorFrame(context,this,statusManager);
		pef.setInitMode(DockContext.STATE_FRAMEDOCKED);
		pef.setInitSide(DockContext.DOCK_SIDE_WEST);
		pef.setInitIndex(10);
		pef.putClientProperty("menu.text", "Diagram Property Editor");
		frames.add(pef);
	}

	@Override
	public String getKey() {
		return key;
	}
	/**
	 * Define the right-click selections for a block on this workspace.
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
				// NOTE: ctypeEditable gets turned off once a block has been serialized.
				if( selection instanceof BlockComponent && pbv.isCtypeEditable() ) {
					
					// Types are: ANY, DATA, TEXT, TRUTH-VALUE
					// Assume the type from the terminus anchor
					Iterator<ProcessAnchorDescriptor> iterator = pbv.getAnchors().iterator();
					ProcessAnchorDescriptor anch = null;  // Assumes at least one outgoing anchor
					while( iterator.hasNext()  ) {
						anch = iterator.next();
						if( anch.getType().equals(AnchorType.Origin) && !anch.getConnectionType().equals(ConnectionType.SIGNAL) ) {
							break;
						}
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
			else if( SchematicDiagramWorkspace.this.getSelectedContainer().getSelectedConnection()!=null ) {
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
		return new SchematicDiagramContainer(this,bdm,this.newEdgeRouter(bdm),this.newConnectionPainter(bdm));
	}
	
	
	
	/**
	 * Paint connections. All connections are signal-style.
	 */
	private class DiagramConnectionPainter extends ArrowConnectionPainter {
		/**
		 * Determine the connection class from the origin, then paint accordingly. 
		 * The origin is a BasicAnchorPoint.
		 */
		@Override
		public void paintConnection(Graphics2D g, Connection cxn,Path2D route, boolean selected, boolean hover) {
			BasicAnchorPoint origin = (BasicAnchorPoint)cxn.getOrigin();
			super.hoverColor = WorkspaceConstants.CONNECTION_HOVER;
			super.selectedColor = WorkspaceConstants.CONNECTION_SELECTED;
			// Signal is different in that it has no fill
			super.stroke = origin.getOutlineStroke();
			super.standardColor=WorkspaceConstants.CONNECTION_FILL_SIGNAL;
			super.paintConnection(g, cxn, route, selected, hover);
		}
	}
}
