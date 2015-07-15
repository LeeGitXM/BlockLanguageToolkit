/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.schematic.workspace;

import java.awt.Graphics2D;
import java.awt.geom.Path2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.editor.PropertyEditorFrame;
import com.ils.blt.designer.schematic.BLTSchematicDesignerHook;
import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockPalette;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceConstants;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
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
	private static final String dockingKey = "SchematicBlockPalette";
	private static final String editorKey = "SchematicPropertyEditorFrame";
	private static final String key = "SchematicDiagramWorkspace";
	/**
	 * Constructor:
	 */
	public SchematicDiagramWorkspace(DesignerContext ctx,ToolkitRequestHandler handler) {
		super(ctx,handler);
		this.statusManager = ((BLTSchematicDesignerHook)context.getModule(BLTProperties.SCHEMATIC_MODULE_ID)).getNavTreeStatusManager();
		initialize();
	}


	// Initialize the workspace frames.
	private void initialize() {
		// Create palette
		ProcessBlockPalette tabbedPalette = new ProcessBlockPalette(context, this,requestHandler,dockingKey);
		tabbedPalette.setInitMode(DockContext.STATE_FRAMEDOCKED);
		tabbedPalette.setInitSide(DockContext.DOCK_SIDE_NORTH);
		tabbedPalette.setInitIndex(0);
		tabbedPalette.setTitle(BundleUtil.get().getString(PREFIX+".Palette.Title"));
		tabbedPalette.setTabTitle(PREFIX+".Palette.Tab.Title");
		tabbedPalette.setSideTitle("SideTitle");
		tabbedPalette.putClientProperty("menu.text", PREFIX+".Palette.Title");
		
		frames = new ArrayList<ResourceWorkspaceFrame>();
		frames.add(tabbedPalette);
		
		PropertyEditorFrame pef = new PropertyEditorFrame(context,this,statusManager,editorKey);
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
	 * Deserialize blocks that were serialized during a "copy" action.
	 * The serialized string was stored on the clipboard.
	 * @return blocks that were recently serialized as a result of a copy.
	 */
	@Override
	public  Collection<Block> pasteBlocks(String json){
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
						logger.debugf("%s: DiagramAction. create new %s resource %d (%d bytes)",TAG,BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE,
								newId,bytes.length);
						ProjectResource resource = new ProjectResource(newId,
								requestHandler.getModuleId(), BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE,
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
