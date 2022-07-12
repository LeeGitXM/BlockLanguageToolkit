/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Stroke;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.config.AttributeDisplaySelector;
import com.ils.blt.designer.config.BlockExplanationViewer;
import com.ils.blt.designer.config.BlockInternalsViewer;
import com.ils.blt.designer.config.ForceValueSettingsDialog;
import com.ils.blt.designer.editor.BlockEditConstants;
import com.ils.blt.designer.editor.BlockPropertyEditor;
import com.ils.blt.designer.editor.PropertyEditorFrame;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.tags.model.ClientTagManager;
import com.inductiveautomation.ignition.client.util.LocalObjectTransferable;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.config.ObservablePropertySet;
import com.inductiveautomation.ignition.common.config.PropertySet;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.sqltags.model.TagProp;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.tags.browsing.NodeBrowseInfo;
import com.inductiveautomation.ignition.common.tags.config.TagConfigurationModel;
import com.inductiveautomation.ignition.common.tags.config.properties.WellKnownTagProps;
import com.inductiveautomation.ignition.common.tags.model.TagPath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.SerializationException;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.blockandconnector.AbstractBlockWorkspace;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockActionHandler;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.ConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.ArrowConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.routing.EdgeRouter;
import com.inductiveautomation.ignition.designer.blockandconnector.undo.UndoMoveBlocks;
import com.inductiveautomation.ignition.designer.designable.DesignPanel;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceListener;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.gui.StatusBar;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.EditActionHandler;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspace;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ProjectBrowserRoot;
import com.inductiveautomation.ignition.designer.tags.tree.dnd.NodeListTransferable;
import com.jidesoft.action.CommandBar;
import com.jidesoft.action.DockableBarManager;
import com.jidesoft.docking.DockContext;
import com.jidesoft.docking.DockingManager;
import com.jidesoft.swing.JideButton;

/**
 * A Diagram workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DockableFrames. These are JIDE components. In addition
 * to a palette and editor, the workspace holds a tabbed panel for holding
 * BlockDesignableContainers (DiagramWorkspaces). These contain the visual 
 * representations of the diagrams.
 */
public class DiagramWorkspace extends AbstractBlockWorkspace 
							  implements ResourceWorkspace, DesignableWorkspaceListener,
							  			ChangeListener                                  {
	private static final String ALIGN_MENU_TEXT = "Align Blocks";
	private static final String CLSS = "DiagramWorkspace";
	private static final boolean DEBUG = false;
	private static final long serialVersionUID = 4627016159409031941L;
	private static final DataFlavor BlockDataFlavor = LocalObjectTransferable.flavorForClass(ObservablePropertySet.class);
	public static final String key = "BlockDiagramWorkspace";
	private static String OS = System.getProperty("os.name").toLowerCase();
	public static final String PREFIX = BLTProperties.BLOCK_PREFIX; 
	private final ApplicationRequestHandler requestHandler;
	private final DesignerContext context;
	private final EditActionHandler editActionHandler;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final NodeStatusManager statusManager;
	private Collection<ResourceWorkspaceFrame> frames;
	private LoggerEx log = LogUtil.getLogger(getClass().getPackageName());
	private final PropertyEditorFrame propertyEditorFrame;
	private PopupListener rightClickHandler;
	private JPopupMenu zoomPopup;
	private JComboBox<String> zoomCombo;
	private CommandBar alignBar = null;

	/**
	 * Constructor:
	 */
	public DiagramWorkspace(DesignerContext ctx) {
		super();
		this.context = ctx;
		this.editActionHandler = new BlockActionHandler(this,context);
		this.addDesignableWorkspaceListener(this);
		this.zoomPopup = createZoomPopup();
		this.rightClickHandler = new PopupListener();
		this.addMouseListener(rightClickHandler);
		this.propertyEditorFrame = new PropertyEditorFrame(context,this);
		this.requestHandler = new ApplicationRequestHandler();
		statusManager = NodeStatusManager.getInstance();
		initialize();
	}


	// Initialize the workspace frames.
	private void initialize() {
		// Create palette
		ProcessBlockPalette tabbedPalette = new ProcessBlockPalette(context,this);
		tabbedPalette.setInitMode(DockContext.STATE_FRAMEDOCKED);
		tabbedPalette.setInitSide(DockContext.DOCK_SIDE_NORTH);
		tabbedPalette.setInitIndex(0);
		tabbedPalette.setTitle(BundleUtil.get().getString(PREFIX+".Palette.Title"));
		tabbedPalette.setTabTitle(BundleUtil.get().getString(PREFIX+".Palette.Tab.Title"));
		tabbedPalette.setSideTitle("SideTitle");
		tabbedPalette.putClientProperty("menu.text", BundleUtil.get().getString(PREFIX+".Palette.Title"));
		
		frames = new ArrayList<ResourceWorkspaceFrame>();
		frames.add(tabbedPalette);
		
		propertyEditorFrame.setInitMode(DockContext.STATE_FRAMEDOCKED);
		propertyEditorFrame.setInitSide(DockContext.DOCK_SIDE_WEST);
		propertyEditorFrame.setInitIndex(10);
		propertyEditorFrame.putClientProperty("menu.text", "Symbolic AI Property Editor");
		frames.add(propertyEditorFrame);
		
		StatusBar bar = context.getStatusBar();
		zoomCombo = createZoomDropDown();
		bar.addDisplay(zoomCombo);
		zoomCombo.setVisible(false);
	}

	@Override
	public EditActionHandler getEditActionHandler() {
		return editActionHandler;
	}

	@Override
	public Collection<ResourceWorkspaceFrame> getFrames() {
		return frames;
	}

	public DesignerContext getContext() { 
		return this.context; 
	}
	
	@Override
	public String getKey() {
		return key;
	}
	/**
	 * @return the dockable frame that supports property editing.
	 */
	public PropertyEditorFrame getPropertyEditorFrame() { return this.propertyEditorFrame; }
	/**
	 * For popup menu. For an attribute display, the only reasonable choice is "delete"
	 */
	@Override
	public JPopupMenu getSelectionPopupMenu(List<JComponent> selections) {
		if( selections.size()>0 ) {
			JComponent selection = selections.get(0);
			if( DEBUG ) log.infof("%s.getSelectionPopupMenu: Component is: %s",CLSS,selections.get(0).getClass().getName());
			if( selection instanceof BlockComponent ) {
				
				JPopupMenu menu = new JPopupMenu();
				
				ProcessBlockView pbv = (ProcessBlockView)((BlockComponent)selection).getBlock();
//				saveAction = new SaveAction(pbv); 
//				// As long as the block is dirty, we can save it. 
//				// NOTE: There is always a corresponding block in the gateway. One is created when we drop block from the palette.
//				saveAction.setEnabled(pbv.isDirty());
//				menu.add(saveAction);
				// NOTE: ctypeEditable gets turned off once a block has been serialized.
				if( selection instanceof BlockComponent && pbv.isCtypeEditable() && 
					!(pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT) ||
					  pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_ATTRIBUTE)) ) {
					
					// Types are: ANY, DATA, TEXT, TRUTH-VALUE
					// Assume the type from the terminus anchor
					Iterator<ProcessAnchorDescriptor> iterator = pbv.getAnchors().iterator();
					ProcessAnchorDescriptor anch = null;  // Assumes at least one outgoing anchor
					while( iterator.hasNext()  ) {
						anch = iterator.next();
						if( anch.getType().equals(AnchorType.Origin) && !anch.getConnectionType().equals(ConnectionType.SIGNAL) ) {
							break;
						}
						// You can change text to anything
						else if( anch.getType().equals(AnchorType.Terminus) && 
								(anch.getConnectionType().equals(ConnectionType.ANY)|| anch.getConnectionType().equals(ConnectionType.TEXT)) ) {
							break;
						}
					}
					
					if( anch!=null ) {
						ConnectionType ct = anch.getConnectionType();
						if( DEBUG ) log.infof("%s.getSelectionPopupMenu: Connection type is: %s",CLSS,ct.name());
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
				if( pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ) {
					log.infof("%s.getSelectionPopupMenu: SINK",CLSS);
					JMenu linkSinkMenu = new JMenu(BundleUtil.get().getString(PREFIX+".FollowConnection.Name"));
					linkSinkMenu.setToolTipText(BundleUtil.get().getString(PREFIX+".FollowConnection.Desc"));
					ProjectResourceId diagramId = getActiveDiagram().getResourceId();
					List<SerializableBlockStateDescriptor> descriptors = requestHandler.listSourcesForSink(diagramId, pbv.getId().toString());
					for(SerializableBlockStateDescriptor desc:descriptors) {
						SerializableResourceDescriptor rd = requestHandler.getDiagramForBlock(desc.getIdString());
						if( rd==null ) continue;
						ShowDiagramAction action = new ShowDiagramAction(rd,desc.getName());
						linkSinkMenu.add(action);
					}
					menu.add(linkSinkMenu);
				}
				else if( pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE) )  {
					log.infof("%s.getSelectionPopupMenu: SOURCE",CLSS);
					JMenu linkSourceMenu = new JMenu(BundleUtil.get().getString(PREFIX+".FollowConnection.Name"));
					linkSourceMenu.setToolTipText(BundleUtil.get().getString(PREFIX+".FollowConnection.Desc"));
					
					ProjectResourceId diagramId = getActiveDiagram().getResourceId();
					List<SerializableBlockStateDescriptor> descriptors = requestHandler.listSinksForSource(diagramId, pbv.getId().toString());
					for(SerializableBlockStateDescriptor desc:descriptors) {
						SerializableResourceDescriptor rd = requestHandler.getDiagramForBlock(desc.getIdString());
						if( rd==null ) continue;
						ShowDiagramAction action = new ShowDiagramAction(rd,desc.getName());
						linkSourceMenu.add(action);
					}
					menu.add(linkSourceMenu);
				}
				if( DEBUG ) log.infof("%s.getSelectionPopupMenu: Selection editor class = %s",CLSS,pbv.getEditorClass());

				// None of the following apply to an attribute display
				if(!pbv.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_ATTRIBUTE) ) {
					// Do not allow editing when the diagram is disabled
					if( !getActiveDiagram().getState().equals(DiagramState.DISABLED)) {
						PropertyDisplayAction cea = new PropertyDisplayAction(getActiveDiagram(),pbv, this);
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
					// Display an explanation if the block is currently TRUE or FALSE

					String currentState = requestHandler.getBlockState(getActiveDiagram().getResourceId(),pbv.getName());
					if( currentState!=null && (currentState.equalsIgnoreCase("true")|| currentState.equalsIgnoreCase("false"))) {
						ExplanationAction act = new ExplanationAction(getActiveDiagram(),pbv);
						menu.add(act);
					}
					ViewInternalsAction via = new ViewInternalsAction(getActiveDiagram(),pbv);
					menu.add(via);
					menu.addSeparator();
					PropagateAction ea = new PropagateAction(pbv);
					menu.add(ea);
					if( !pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_INPUT)) {
						TriggerUpstreamPropagationAction eaup = new TriggerUpstreamPropagationAction(pbv);
						menu.add(eaup);
					}
					ForceAction fa = new ForceAction(getActiveDiagram(),pbv);
					menu.add(fa);
					HelpAction ha = new HelpAction(pbv);
					menu.add(ha);
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

					menu.add(new SelectAllBlocksAction(this,getActiveDiagram(),pbv));
					if  (selections.size() > 1) {
						AlignLeftAction al = new AlignLeftAction(this,getActiveDiagram(),pbv, selections);
						menu.add(al);
						AlignRightAction ar = new AlignRightAction(this,getActiveDiagram(),pbv, selections);
						menu.add(ar);
						AlignWidthCenterAction aw = new AlignWidthCenterAction(this,getActiveDiagram(),pbv, selections);
						menu.add(aw);
						AlignTopAction at = new AlignTopAction(this,getActiveDiagram(),pbv, selections);
						menu.add(at);
						AlignBottomAction ab = new AlignBottomAction(this,getActiveDiagram(),pbv, selections);
						menu.add(ab);
						AlignHeightCenterAction ah = new AlignHeightCenterAction(this,getActiveDiagram(),pbv, selections);
						menu.add(ah);
					}
				}
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
	protected EdgeRouter newEdgeRouter(BlockDiagramModel bdm) {
		ProcessDiagramView mdl = (ProcessDiagramView)bdm;
		if( mdl.getConnections().size()>15) {
			return new HighPerformanceEdgeRouter();
		}
		else {
			return super.newEdgeRouter(bdm);
		}
	}
	
	@Override
	public MenuBarMerge getMenu() {
    	BundleUtil.get();

    	MenuBarMerge merge = null;     	
    	if (this.isVisible()) {
    		if (!menuExists(context.getFrame(),ALIGN_MENU_TEXT)) {

    			JMenuItem la = new JMenuItem(new AbstractAction("Left Align") {
    				private static final long serialVersionUID = 1L;
		    		public void actionPerformed(ActionEvent e) {
		    			alignLeft();
		    		}
	        	});
    			JMenuItem ra = new JMenuItem(new AbstractAction("Right Align") {
    				private static final long serialVersionUID = 1L;
		    		public void actionPerformed(ActionEvent e) {
		    			alignRight();
		    		}
	        	});
    			JMenuItem ha = new JMenuItem(new AbstractAction("Horizontal Center Align") {
    				private static final long serialVersionUID = 1L;
		    		public void actionPerformed(ActionEvent e) {
		    			alignWidthCenter();
		    		}
	        	});
    			JMenuItem ta = new JMenuItem(new AbstractAction("Top Align") {
    				private static final long serialVersionUID = 1L;
		    		public void actionPerformed(ActionEvent e) {
		    			alignTop();
		    		}
	        	});
    			JMenuItem ba = new JMenuItem(new AbstractAction("Bottom Align") {
    				private static final long serialVersionUID = 1L;
		    		public void actionPerformed(ActionEvent e) {
		    			alignBottom();
		    		}
	        	});
    			JMenuItem va = new JMenuItem(new AbstractAction("Vertical Center Align") {
    				private static final long serialVersionUID = 1L;
		    		public void actionPerformed(ActionEvent e) {
		    			alignHeightCenter();
		    		}
	        	});
    			
    			merge = new MenuBarMerge(BLTProperties.MODULE_ID);  // as suggested in javadocs
		    	JMenuMerge alignmentMenu = new JMenuMerge("Align Blocks",BLTProperties.BUNDLE_PREFIX+".Menu.AlignBlocks");
		    	// "alignblocks" needs to be in the resource bundle or else it gets ? added to it.
		    	alignmentMenu.add(ra);
		    	alignmentMenu.add(la);
		    	alignmentMenu.add(ha);
		    	alignmentMenu.add(ta);
		    	alignmentMenu.add(ba);
		    	alignmentMenu.add(va);
		    	merge.add(alignmentMenu);
    		}
    	} else {
    		removeTopLevelMenu(context.getFrame(),ALIGN_MENU_TEXT);
    	}
    	
    	return merge;
	}
	
	// Added by PH 06/30/2021
	public NodeStatusManager getNodeStatusManager(){
		return this.statusManager;
	}
	
	/**
	 * Post an internals viewer for the block. The default shows
	 * only name, class and UUID. Blocks may transmit additional
	 * parameters as is useful. 
	 */
	private class PropertyDisplayAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessDiagramView diagram;
		private final ProcessBlockView block;
		private final DiagramWorkspace workspace;
		public PropertyDisplayAction(ProcessDiagramView dia,ProcessBlockView blk, DiagramWorkspace wkspc)  {
			super(PREFIX+".DisplayProperties",IconUtil.getIcon("sun"));
			this.workspace = wkspc;
			this.diagram = dia;
			this.block = blk;
		}
		
		// Display the internals viewer
		public void actionPerformed(final ActionEvent e) {
			final JDialog viewer = (JDialog)new AttributeDisplaySelector(context.getFrame(),diagram,block);
			
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


	// Search the menu tree and remove the selected item
	private void removeTopLevelMenu(Frame frame,String title) {
		for(Component c:context.getFrame().getComponents() ) {
    		if( c instanceof JRootPane ) {
    			JRootPane root = (JRootPane)c;
    			JMenuBar bar = root.getJMenuBar();
    			if( bar!=null ) {
    				int count = bar.getMenuCount();
    				int index = 0;
    				while( index<count) {
    					JMenu menu = bar.getMenu(index);
    					if( menu.getName().equalsIgnoreCase(title)) {
    						bar.remove(menu);
    						break;
    					}
    					index++;
    				}
    			}
    		}
    	}
		return;
	}

	// Search the menu tree to see if the same top level menu has been added already
	private boolean menuExists(Frame frame,String title) {
		boolean ret = false;
		for(Component c:context.getFrame().getComponents() ) {
    		if( c instanceof JRootPane ) {
    			JRootPane root = (JRootPane)c;
    			JMenuBar bar = root.getJMenuBar();
    			if( bar!=null ) {
    				int count = bar.getMenuCount();
    				int index = 0;
    				while( index<count) {
    					JMenu menu = bar.getMenu(index);
    					if( menu.getName().equalsIgnoreCase(title)) {
    						ret = true;
    						break;
    					}
    					index++;
    				}
    			}
    		}
    	}
		
		return ret;
	}



	public JideButton makeAlignButton(String imgName, String toolTip, ActionListener action) {
		JideButton btn = new JideButton();
		String iconPath  = "Block/icons/editor/" + imgName;
		Image img = ImageLoader.getInstance().loadImage(iconPath ,BlockEditConstants.BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn.setIcon(icon);
			btn.setToolTipText(toolTip);
			btn.addActionListener(action);
		}
		return btn;
	}
	

	// List of toolbars to add - EREIAM JH - This is called when exiting the diagram workspace, but not seemingly when it is entered.  weird
	//  Nothing seems to show up anyway...
	@Override
	public List<CommandBar> getToolbars() {
		ArrayList<CommandBar> bars = new ArrayList<>();
		alignBar = new CommandBar();
		alignBar.setKey("bltAlign");

		ActionListener align = new ActionListener() { public void actionPerformed(ActionEvent e){ alignLeft();	} };
		alignBar.add(makeAlignButton("align_left.png", "Align blocks by their left edge", align));
		align = new ActionListener() { public void actionPerformed(ActionEvent e){ alignRight();} };
		alignBar.add(makeAlignButton("align_right.png", "Align blocks by their right edge", align));
		align = new ActionListener() { public void actionPerformed(ActionEvent e){ alignTop();} };
		alignBar.add(makeAlignButton("align_top.png", "Align blocks by their top edge", align));
		align = new ActionListener() { public void actionPerformed(ActionEvent e){ alignBottom();} };
		alignBar.add(makeAlignButton("align_bottom.png", "Align blocks by their bottom edge", align));
		align = new ActionListener() { public void actionPerformed(ActionEvent e){ alignHeightCenter();} };
		alignBar.add(makeAlignButton("align_horizontal.png", "Align blocks by their horizontal centerpoint", align));
		align = new ActionListener() { public void actionPerformed(ActionEvent e){ alignWidthCenter();} };
		alignBar.add(makeAlignButton("align_vertical.png", "Align blocks by their vertical centerpoint", align));

		bars.add(alignBar);
		return bars;
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
		JComponent ret = this;
		for (JComponent thingy:itemsUnderDrop) {
			if (thingy instanceof BlockComponent) {
				ret = thingy;
				break;
			}
		}
		return ret;
	}
	
	// This is the generic drop handling
	@Override
	public boolean handleDrop(Object droppedOn,DropTargetDropEvent event) {
		if (droppedOn instanceof BlockComponent) {
			log.infof("%s.handleDrop: dropped on block component",CLSS);
			handleTagOnBlockDrop(droppedOn, event);
		}
		else if (droppedOn instanceof DiagramWorkspace) {
			log.infof("%s.handleDrop: dropped on diagram, checking for tag",CLSS);    // This handles case of dropee being a tag
			handleTagDrop(droppedOn, event);
		}
		if (event.isDataFlavorSupported(BlockDataFlavor)) {        // Handle block dropped from palette
			try {
				if( event.getTransferable().getTransferData(BlockDataFlavor) instanceof ProcessBlockView) {
					ProcessBlockView block = (ProcessBlockView)event.getTransferable().getTransferData(BlockDataFlavor);
					DesignPanel panel = getSelectedDesignPanel();
					BlockDesignableContainer bdc = (BlockDesignableContainer) panel.getDesignable();
					Point dropPoint = SwingUtilities.convertPoint(
							event.getDropTargetContext().getComponent(),
							panel.unzoom(event.getLocation()), bdc);
					this.setCurrentTool(getSelectionTool());   // So the next select on workspace does not result in another block
					if( isInBounds(dropPoint,bdc) ) {
						block.setLocation(dropPoint);
						this.getActiveDiagram().addBlock(block);
						log.infof("%s.handleDrop: dropped block %s",CLSS,block.getClassName());
						if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ||
							block.getClassName().equals(BlockConstants.BLOCK_CLASS_INPUT) ||
							block.getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT) ||
							block.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) {
							addNameDisplay(block,dropPoint.x,dropPoint.y);
						}
						// Create the process editor for the new block
						BlockPropertyEditor editor = new BlockPropertyEditor(context,this,block);
						PropertyEditorFrame peframe = getPropertyEditorFrame();
						peframe.setEditor(editor);
						
						return true;
					}
					else {
						log.infof("%s.handleDrop: drop of %s out-of-bounds",CLSS,event.getTransferable().getTransferData(BlockDataFlavor).getClass().getName());
					}
				}
				else {
					log.infof("%s.handlerDrop: Unexpected class (%s),  rejected",
							event.getTransferable().getTransferData(BlockDataFlavor).getClass().getName());
				}
			} 
			catch (Exception e) {
				ErrorUtil.showError(CLSS+" Exception handling drop",e);
			}
		}
		return false;
	}


	// Check to see if this is a tag dropped on the workspace.  Make it an Input block if it's on the left, 
	// Output on the right half -- unless the tagpath starts with "Connections". Then we make
	// Sources/Sinks
	private void handleTagDrop(Object droppedOn, DropTargetDropEvent event) {
		DataFlavor flava = NodeListTransferable.FLAVOR_NODE_INFO_LIST;
		if (event.isDataFlavorSupported(flava)) {
			try {
				Object node = event.getTransferable().getTransferData(flava);
				if (node instanceof ArrayList && ((List<?>) node).size() == 1) {
					ArrayList<?> tagNodeArr = (ArrayList<?>)node;
					// NOTE: This will fail (properly) if tag type is a dataset
					if (tagNodeArr.get(0) instanceof NodeBrowseInfo) {  // That's the thing we want!
						BlockDesignableContainer container = getSelectedContainer();
						ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
						NodeBrowseInfo tnode = (NodeBrowseInfo) tagNodeArr.get(0);
						int dropx = event.getLocation().x;
						int dropy = event.getLocation().y;
						int thewidth = getActiveDiagram().getDiagramSize().width;

						if( getSelectedContainer()!=null ) {
							ProcessBlockView block = null;
							TagPath tp = tnode.getFullPath();
							boolean isStandardFolder = BusinessRules.isStandardConnectionsFolder(tp);
							BlockDescriptor desc = new BlockDescriptor();
							// Sinks to right,sources to the left
							if (dropx < thewidth / 2) {	
								if( isStandardFolder ) {
									desc.setBlockClass(BlockConstants.BLOCK_CLASS_SOURCE);
									desc.setStyle(BlockStyle.ARROW);
									desc.setPreferredHeight(40);
									desc.setPreferredWidth(50);
									desc.setBackground(new Color(127,127,127).getRGB()); // Dark gray
									desc.setCtypeEditable(true);
									block = new ProcessBlockView(desc);
									block.setName(nameFromTagTree(tnode));
									updatePropertiesForTagPath(block,tnode.getFullPath().toStringFull());
								}
								else {
									desc.setBlockClass(BlockConstants.BLOCK_CLASS_INPUT);
									desc.setStyle(BlockStyle.ARROW);
									desc.setPreferredHeight(46);
									desc.setPreferredWidth(60);
									desc.setBackground(Color.cyan.getRGB());
									desc.setCtypeEditable(true);
									block = new ProcessBlockView(desc);
									block.setName(enforceUniqueName(nameFromTagTree(tnode),diagram));
									updatePropertiesForTagPath(block,tnode.getFullPath().toStringFull());
								}
								// Define a single output
								AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
								block.addAnchor(output);
								// Properties are the same for Inputs and Sources
								// This property causes the engine to start a subscription.
								BlockProperty tagPathProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,tnode.getFullPath().toStringFull(),PropertyType.OBJECT,true);
								tagPathProperty.setBinding(tnode.getName());
								tagPathProperty.setBindingType(BindingType.TAG_READ);
								block.setProperty(tagPathProperty);
								
								BlockProperty valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
								valueProperty.setBindingType(BindingType.ENGINE);
								block.setProperty(valueProperty);								
							} 
							else {
								if( isStandardFolder ) {
									desc.setBlockClass(BlockConstants.BLOCK_CLASS_SINK);
									desc.setPreferredHeight(40);
									desc.setPreferredWidth(50);    // Leave 6-pixel inset on top and bottom
									desc.setBackground(new Color(127,127,127).getRGB()); // Dark gray
									desc.setStyle(BlockStyle.ARROW);
									desc.setCtypeEditable(true);
									block = new ProcessBlockView(desc);
									block.setName(nameFromTagTree(tnode));
									// Define a single output
									AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
									block.addAnchor(output);
								}
								else {
									desc.setBlockClass(BlockConstants.BLOCK_CLASS_OUTPUT);
									desc.setStyle(BlockStyle.ARROW);
									desc.setPreferredHeight(46);
									desc.setPreferredWidth(60);
									desc.setBackground(Color.cyan.getRGB());
									desc.setCtypeEditable(true);
									block = new ProcessBlockView(desc);
									block.setName(enforceUniqueName(nameFromTagTree(tnode),diagram));
								}	
								 
								// Define a single input
								AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
								input.setIsMultiple(false);
								block.addAnchor(input);
								// Properties are the same for Outputs and Sinks
								BlockProperty pathProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
								pathProperty.setBindingType(BindingType.TAG_WRITE);
								pathProperty.setBinding(tnode.getFullPath().toStringFull());
								block.setProperty( pathProperty);
								BlockProperty valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
								valueProperty.setBindingType(BindingType.ENGINE);
								block.setProperty(valueProperty);
							}
							
							AnchorPrototype signal = new AnchorPrototype(BlockConstants.SIGNAL_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.SIGNAL);
							block.addAnchor(signal);
							
							// Define a property that holds the size of the activity buffer. This applies to all blocks.
							BlockProperty bufferSize = new BlockProperty(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE,10,PropertyType.INTEGER,true);
							block.setProperty(bufferSize);
							
							DesignPanel panel = getSelectedDesignPanel();
							BlockDesignableContainer bdc = (BlockDesignableContainer) panel.getDesignable();
							Point dropPoint = SwingUtilities.convertPoint(
									event.getDropTargetContext().getComponent(),
									panel.unzoom(event.getLocation()), bdc);
							this.setCurrentTool(getSelectionTool());   // So the next select on workspace does not result in another block

							if( isInBounds(dropPoint,bdc) ) {
								block.setLocation(dropPoint);
								this.getActiveDiagram().addBlock(block);
								addNameDisplay(block,dropx,dropy);
								DataType type = DataType.Boolean;
								List<TagPath> paths = new ArrayList<>();
								paths.add(tnode.getFullPath());
								ClientTagManager tmgr = context.getTagManager();
								CompletableFuture<List<TagConfigurationModel>> futures = tmgr.getTagConfigsAsync(paths,false,true);
								try {
									TagConfigurationModel model = futures.get().get(0);
									PropertySet config = model.getTagProperties();
									type = (DataType)config.getOrDefault(WellKnownTagProps.DataType);
								}
								catch(Exception ex) {
									log.infof("%s.handleTagDrop: failed to get tag info for %s %s (%s)",CLSS,block.getClassName(),tnode.getFullPath().toStringFull(),
											ex.getMessage());
								}


								Collection<BlockProperty> props = block.getProperties();
								for (BlockProperty property:props) {
									if( BlockConstants.BLOCK_PROPERTY_TAG_PATH.equalsIgnoreCase(property.getName())) {
										property.setBinding(tnode.getFullPath().toStringFull());}
									block.modifyConnectionForTagChange(property, type);
								}
								
								// Create the process editor for the new block
								BlockPropertyEditor editor = new BlockPropertyEditor(context,this,block);
								PropertyEditorFrame peframe = getPropertyEditorFrame();
								peframe.setEditor(editor);
								log.infof("%s.handleTagDrop: dropped %s (%s)",CLSS,block.getClassName(),block.getName());
							}
							else {
								log.infof("%s.handleTagDrop: drop of %s out-of-bounds",CLSS,block.getClassName());
							}
						}
						getActiveDiagram().fireStateChanged();
					}
				}
			} 
			catch (UnsupportedFlavorException e) {
				//ignore
			} 
			catch (IOException e) {
				// ignore
			}
		}

	}


	// Check to see if this is a tag dropped on a block.  Change the tag binding if applicable.
	// Dropping the tag changes the block name and connection type if not "locked".
	// For a source/sink, tag must be in "Connections". For Input/Output it must not.
	// Craig is correct in that there is an amount of ugliness that involves hard-coded block class names
	// and properties
	public void handleTagOnBlockDrop(Object droppedOn, DropTargetDropEvent event) {
		try {
			DataFlavor flava = NodeListTransferable.FLAVOR_NODE_INFO_LIST;
			if (event.isDataFlavorSupported(flava)) {
				Object node = event.getTransferable().getTransferData(flava);
				if (node instanceof ArrayList && ((ArrayList<?>) node).size() == 1) {
					ArrayList<?> tagNodeArr = (ArrayList<?>)node;
					if (tagNodeArr.get(0) instanceof NodeBrowseInfo) {  // That's the thing we want!
						BlockDesignableContainer container = getSelectedContainer();
						ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
						NodeBrowseInfo tnode = (NodeBrowseInfo) tagNodeArr.get(0);
						log.infof("%s.handleTagOnBlockDrop: tag data: %s",CLSS,tnode.getFullPath());
						TagPath tp = tnode.getFullPath();

						Block targetBlock = ((BlockComponent)droppedOn).getBlock();
						if(targetBlock instanceof ProcessBlockView)  {
							ProcessBlockView pblock = (ProcessBlockView)targetBlock;
							
							BlockProperty prop = pblock.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
							if( prop==null) return;  // Unless there's a tag path, do nothing
							
							DataType tagType = DataType.Boolean;
							Integer exprType = 0;
							ClientTagManager tmgr = context.getTagManager();
							List<TagPath> paths = new ArrayList<>();
							paths.add(tp);
							CompletableFuture<List<TagConfigurationModel>> futures = tmgr.getTagConfigsAsync(paths, false, true);
							// There should be only one model as there was only o
							try {
								List<TagConfigurationModel> results = futures.get();
								TagConfigurationModel model = results.get(0);
								PropertySet config = model.getTagProperties();
								tagType = (DataType)config.getOrDefault(WellKnownTagProps.DataType);
								exprType = (Integer) config.get(TagProp.ExpressionType);
							}
							catch(Exception ex) {
								log.infof("%s.handleDiagramDrop: failed to get tag info for %s (%s)",CLSS,tnode.getFullPath().toStringFull(),
										ex.getMessage());
							}
							String connectionMessage = diagram.isValidBindingChange(pblock, prop, tp.toStringFull(), tagType,exprType);
							
							if( connectionMessage==null ) {
								prop.setBinding(tnode.getFullPath().toStringFull());
								setSelectedItems((JComponent)null);  // hack to get the property panel to refresh
								setSelectedItems((JComponent)droppedOn);
								pblock.setName(nameFromTagTree(tnode));
								pblock.setCtypeEditable(true);
								pblock.modifyConnectionForTagChange(prop, tagType);
								getActiveDiagram().fireStateChanged();
							} 
							else {
								JOptionPane.showMessageDialog(null, connectionMessage, "Warning", JOptionPane.INFORMATION_MESSAGE);
							}
						}
					}
				}
			}
		}
		catch(Exception ex) {
			log.error(String.format("%s.handleDrop: Exceptiona: %s",CLSS,ex.getLocalizedMessage()),ex);
		}
	}

	/**
	 * Add an attribute display of the name to the specified block.
	 * @param diagram
	 * @param block
	 */
	private void addNameDisplay(ProcessBlockView block,int x,int y) {
		BlockAttributeView bav = new BlockAttributeView(new AttributeDisplayDescriptor(),block.getId().toString());
		bav.setReferenceBlock(block);
		bav.setPropertyName(BlockConstants.BLOCK_PROPERTY_NAME);
		bav.startListener();
		bav.setValue(block.getName());
		bav.setFormat("%s");

		Point loc = new Point(x+BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_X,
                y+block.getPreferredHeight()+BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_Y);
		this.getActiveDiagram().addBlock(bav);
		log.infof("%s.addNameDisplay: %s",CLSS,block.getName());
		SwingUtilities.invokeLater(new BlockPositioner(this,bav,loc));
	}
	
	@Override
	public void onActivation() {
		zoomCombo.setVisible(true);
		log.infof("%s.onActivation",CLSS);
		
	}


	@Override
	public void onDeactivation() {
		zoomCombo.setVisible(false);
		log.infof("%s.onDeactivation",CLSS);
	}
	
	// Guarantee a unique name for a block that has not yet been added to the diagram.
	public String enforceUniqueName(String name,ProcessDiagramView diagram) {
		int count = 0;
		for(Block block:diagram.getBlocks() ) {
			if(block instanceof ProcessBlockView) {
				String bname = ((ProcessBlockView)block).getName();
				if( bname.startsWith(name+"-") ) {
					String suffix = bname.substring(name.length()+1);
					if( suffix==null) count = count+1;  // Name ends in -
					else {
						try {
							int val = Integer.parseInt(suffix);
							if( val>=count) count = val + 1;
							else count = count+1;
						}
						catch(NumberFormatException nfe) {
							count = count + 1;
						}
					}
				}
				// The plain name
				else if( bname.equalsIgnoreCase(name) ) {
					count = count+1;
				}
			}
		}
		if( count>0 ) {
			name = name +"-"+String.valueOf(count);
		}
		return name;
	}
	
	private String nameFromTagTree(NodeBrowseInfo tnode) {
		String name = tnode.getName();
		return name;
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
		log.infof("%s: copyBlocks",CLSS);
		ObjectMapper mapper = new ObjectMapper();
		String json = null;
		List<SerializableBlock> list = new ArrayList<SerializableBlock>();
		for( Block blk:blocks) {
			log.infof("%s: copyBlocks class=%s",CLSS,blk.getClass().getName());
			ProcessBlockView view = (ProcessBlockView)blk;
			SerializableBlock sb = view.convertToSerializable();
			list.add(sb);
		}
		try{ 
			   json = mapper.writeValueAsString(list);
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s: Unable to serialize block list (%s)",CLSS,jpe.getMessage());
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
		log.infof("%s.pasteBlocks: %s",CLSS,json);
		ObjectMapper mapper = new ObjectMapper();
		Collection<Block>results = new ArrayList<Block>();
		JavaType type = mapper.getTypeFactory().constructCollectionType(ArrayList.class, SerializableBlock.class);
		try {
			List<SerializableBlock>list = mapper.readValue(json, type);

			Point offset = calculatePasteOffset(this.getMousePosition(), list);
			ProcessDiagramView theDiagram = getActiveDiagram();
			for(SerializableBlock sb:list) {
				ProcessBlockView pbv = new ProcessBlockView(sb);
				
//				if it's a diagnosis rename with similar and then copy aux
				if (pbv.isDiagnosis()) {
//					diagnosis = true;
//					String oldName = new String(pbv.getName());
					pbv.createPseudoRandomNameExtension();
					pbv.getAuxiliaryData().getProperties().put("Name", pbv.getName());   // Use as a key when fetching
				} 
				else {
					pbv.createPseudoRandomName();
				}
				pbv.createRandomId();
				Point dropLoc = new Point(pbv.getLocation().x+offset.x, pbv.getLocation().y+offset.y);
				pbv.setLocation(dropLoc);
				if (pbv.isDiagnosis()) {
				}
				
				
//				if it's a diagnosis it gets renamed, but it doesn't get the aux data from the original saved.
//				Should this read in the aux data from the source block and copy it ?
			
				results.add(pbv);
			}
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s: pasteBlocks parse exception (%s)",CLSS,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s: pasteBlocks mapping exception (%s)",CLSS,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s: pasteBlocks IO exception (%s)",CLSS,ioe.getLocalizedMessage());
		}; 
		return results;
	}

	

	private Point calculatePasteOffset(Point mousePos, List<SerializableBlock> list) {
		int leftPos = 100000;  //just initialize high for simplicity
		int topPos = 100000;

		for(SerializableBlock sb:list) {
			ProcessBlockView pbv = new ProcessBlockView(sb);
			if (pbv.getLocation().x < leftPos) {
				leftPos = pbv.getLocation().x;
			}
			if (pbv.getLocation().y < topPos) {
				topPos = pbv.getLocation().y;
			}
		}
		Point offset = new Point(mousePos.x - leftPos, mousePos.y - topPos);
		return offset;
	}

	// Return the upper corner and max size of the selected objects
	//    Size is handy for centering
	private Rectangle findTopLeft(Collection<BlockComponent> list) {
		int leftPos = 100000;  //just initialize high for simplicity
		int topPos = 100000;
		int height = 0;
		int width = 0;

		for(BlockComponent sb:list) {
//			ProcessBlockView pbv = new ProcessBlockView(sb);
			if (sb.getLocation().x < leftPos) {
				leftPos = sb.getLocation().x;
			}
			if (sb.getLocation().y < topPos) {
				topPos = sb.getLocation().y;
			}
			if (sb.getHeight() > height) {
				height = sb.getHeight();
			}
			if (sb.getWidth() > width) {
				width = sb.getWidth();
			}
		}
		Rectangle r = new Rectangle(leftPos, topPos, width, height);
		return r;
	}

	private Rectangle findBottomRight(Collection<BlockComponent> list) {
		int rightPos = 0;  
		int bottomPos = 0;
		int height = 0;
		int width = 0;

		for(BlockComponent sb:list) {
//			ProcessBlockView pbv = new ProcessBlockView(sb);
			if (sb.getLocation().x+sb.getWidth() > rightPos) {
				rightPos = sb.getLocation().x+sb.getWidth();
			}
			if (sb.getLocation().y+sb.getHeight() > bottomPos) {
				bottomPos = sb.getLocation().y+sb.getHeight();
			}
			if (sb.getHeight() > height) {
				height = sb.getHeight();
			}
			if (sb.getWidth() > width) {
				width = sb.getWidth();
			}
		}
		Rectangle r = new Rectangle(rightPos, bottomPos, height, width);
		return r;
	}
	
	private Collection<BlockComponent> getSelectedBlocks() {
		Collection<BlockComponent> results = new ArrayList<BlockComponent>();
		List<JComponent> components = this.getSelectedItems();
		
		for (JComponent block:components) {
			if (block instanceof BlockComponent) {
				results.add((BlockComponent)block);
			}
		}
		return results;
	}
	
	private void selectAllBlocks() {

	    BlockDesignableContainer container = getSelectedContainer();
	    Component[] blocks = container.getComponents();

		ArrayList<JComponent> blockList = new ArrayList<>();
		for( Component block:blocks) {
			
			if (block instanceof BlockComponent) {
				((BlockComponent) block).setSelected(true);
				blockList.add((BlockComponent)block);
				this.setSelectedItems(blockList);
			}
		}

		SwingUtilities.invokeLater(new WorkspaceRepainter());
		return;
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
	 * @return the currently active (open) diagram, else null
	 */
	public ProcessDiagramView getActiveDiagram() {
		ProcessDiagramView view =null;
		if(getSelectedContainer()!=null ) {
			view = (ProcessDiagramView)(getSelectedContainer().getModel());
		}
		return view; 
	}
	
	/**
	 * On open, use in this order:
	 * 1) The diagram already displayed
	 * 2) The "dirty" diagram saved in the status manager
	 * 3) The diaram deserialized from the project resource
	 * @param resourceId
	 */
	public void open (ProjectResourceId resourceId) {
		if( DEBUG ) log.infof("%s: open - already open (%s)",CLSS,(isOpen(resourceId.getResourcePath())?"true":"false"));
		
		if(isOpen(resourceId.getResourcePath()) ) {
			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId.getResourcePath());
			open(tab);  // Brings tab to front
		}
		else {
			ProcessDiagramView diagram = null;
			if(statusManager.getPendingView(resourceId)!=null ) {
				// This is a changed version, do not update from gateway
				diagram = statusManager.getPendingView(resourceId);
			}
			else {
				Optional<ProjectResource> option = context.getProject().getResource(resourceId);
				ProjectResource res = option.get();
				if( res==null || res.getData()==null ) {
					log.warnf("%s.open - resource is null or has no data (%s)",CLSS,resourceId.getResourcePath().getFolderPath());
					return;
				}
				String json = new String(res.getData());
				if( DEBUG ) log.infof("%s.open: diagram = %s",CLSS,json);
				SerializableDiagram sd = null;
				ObjectMapper mapper = new ObjectMapper();
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);  //
				mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true); // Allows obsolete enum values to pass
				try {
					sd = mapper.readValue(json,SerializableDiagram.class);
					// Synchronize names as the resource may have been re-named and/or
					// state changed since it was serialized. Nulls are ignored.
					sd.setName(resourceId.getResourcePath().getName());
					sd.setName(statusManager.getPendingName(res.getResourceId()));   // If null will not override
					sd.setState(statusManager.getPendingState(res.getResourceId()));
					if( DEBUG ) {
						for(SerializableBlock sb:sd.getBlocks()) {
							log.infof("%s: %s block, name = %s",CLSS,sb.getClassName(),sb.getName());
						}
					}
				} 
				catch (JsonParseException jpe) {
					log.warnf("%s.open: JSON parse exception (%s)",CLSS,jpe.getLocalizedMessage());
				} 
				catch (JsonMappingException jme) {
					log.warnf("%s.open: JSON mapping exception (%s)",CLSS,jme.getLocalizedMessage());
				} 
				catch (IOException ioe) {
					log.warnf("%s.open: io exception (%s)",CLSS,ioe.getLocalizedMessage());
				}
				diagram = new ProcessDiagramView(context,res.getResourceId(),sd);
				// We are looking at the live version running in the gateway, obtain any updates
				for( Block blk:diagram.getBlocks()) {
					ProcessBlockView pbv = (ProcessBlockView)blk;
					pbv.initProperties(res.getResourceId());
				}
			}
			
			// Now we have the view, display it -------------------------------
			super.open(diagram);

			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId.getResourcePath());
			tab.setBackground(diagram.getBackgroundColorForState());
			tab.setName(diagram.getDiagramName());
			// For some unknown reason, the viewport comes up with a red border. Make it gray
			log.infof("%s.open %s is %s",CLSS,tab.getParent().getParent().getParent().getClass().getCanonicalName(),tab.getParent().getParent().getParent().getBackground());
			SwingUtilities.invokeLater(new Runnable() {
			    public void run() {
			     tab.getParent().getParent().getParent().setBackground(Color.GRAY);
			     log.infof("%s.open %s is %s",CLSS,tab.getParent().getParent().getParent().getClass().getCanonicalName(),tab.getParent().getParent().getParent().getBackground());
			    }
			  });
			// Only register for status updates if the diagram is "clean"
			if(statusManager.getPendingView(resourceId)==null ) {
				diagram.registerChangeListeners();
				requestHandler.triggerStatusNotifications(diagram.getResourceId().getProjectName());
			}
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	// On close we can save the container, no questions asked with a
	// call to: saveDiagram((BlockDesignableContainer)container);
	// As it is ... a dialog pops up.
	@Override
	protected void onClose(DesignableContainer c) {
		if( DEBUG ) log.infof("%s: onClose",CLSS);
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
		DiagramTreeNode node = (DiagramTreeNode)statusManager.getNode(diagram.getResourceId());
		if( node!=null ) {
			node.setIcon(node.getIcon());
			node.refresh();
		}
		diagram.unregisterChangeListeners();
		
	}
	/**
	 * The selection has changed. If we've made a change to the currently active diagram,
	 * set its background accordingly. The diagram should have set its own state.
	 * Also update the navtree.
	 */
	private void updateBackgroundForDiagramState() {
		BlockDesignableContainer container = getSelectedContainer();
		if( container!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)(container.getModel());			
			container.setBackground(view.getBackgroundColorForState());
			statusManager.getNode(view.getResourceId()).select();
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	public void close (ProjectResourceId resourceId) {
		log.infof("%s: close resource %s",CLSS,resourceId.getFolderPath());
		super.close(findDesignableContainer(resourceId.getResourcePath()));
	}
	
	// =========================== DesignableWorkspaceListener ===========================
	@Override
	public void containerClosed(DesignableContainer c) {
		log.infof("%s.containerClosed: %s",CLSS,c.getName());
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView view = (ProcessDiagramView)(container.getModel());
		view.removeChangeListener(this);
		container.removeAll();
	}
	/**
	 * Container layout manager is:
	 * com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer$BlockLayout
	 */
	@Override
	public void containerOpened(DesignableContainer c) {
		log.infof("%s.containerOpened: %s",CLSS,c.getName());
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView view = (ProcessDiagramView)(container.getModel());
		view.addChangeListener(this);
	}
	@Override
	public void containerSelected(DesignableContainer container) {
		if( container==null ) log.infof("%s.containerSelected is null",CLSS);
		else{
			log.infof("%s.containerSelected: %s, updating background color...",CLSS,container.getName());
			// Added to clear the dirty background if more than one diagram was dirty before the save. 
			// When we save, we save every processDiagramView that is dirty and then we clear the dirty flag, but only the diagram that is on top got PAH 7/18/2021
			updateBackgroundForDiagramState();
		}
	}
	public CommandBar getAlignBar() {
		return alignBar;
	}
	@Override
	public void itemSelectionChanged(List<JComponent> selections) {
		boolean enableAlign = false;
		if( selections!=null ) {
			if (selections.size()==1 ) {
				JComponent selection = selections.get(0);
				log.infof("%s.itemSelectionChanged: selected a %s",CLSS,selection.getClass().getCanonicalName());
			} 
			else {
				int count = 0;
				for (JComponent cp:selections) {
					if (cp instanceof BlockComponent) {
						count += 1;
					}
				}
				if (count > 1) {
					enableAlign = true;
				}
			}
		}
		updateBlockAlignMenus(enableAlign);
	}
	
	private void updateBlockAlignMenus(boolean enableAlign) {
//		BLTDesignerHook dh = (BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID);
		CommandBar cb = getAlignBar();
		for (Component cp:cb.getComponents()) {
			cp.setEnabled(enableAlign);
		}
	}
	
	/**
	 * Update the tag path property for a new path. While we're at it we publicize the name
	 * @param diagram
	 * @param block
	 */
	private void updatePropertiesForTagPath(ProcessBlockView block,String path) {
		String name = block.getName();
		String nameKey = NotificationKey.keyForBlockName(block.getId().toString());
		notificationHandler.initializeBlockNameNotification(nameKey,name);
		BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
		if( prop !=null ) {
			prop.setBinding(path);
			String propKey = NotificationKey.keyForPropertyBinding(block.getId().toString(), prop.getName());
			notificationHandler.initializePropertyBindingNotification(propKey, path);
		}
	}

	private boolean isInBounds(Point dropPoint,BlockDesignableContainer bdc) {
		Rectangle bounds = bdc.getBounds();
		boolean inBounds = true;
		if( dropPoint.x<bounds.x      ||
			dropPoint.y<bounds.y	  ||
			dropPoint.x>bounds.x+bounds.width ||
			dropPoint.y>bounds.y+bounds.height   )  inBounds = false;
		//log.infof("%s.isInBounds: drop x,y = (%d,%d), bounds %d,%d,%d,%d",CLSS,dropPoint.x,dropPoint.y,bounds.x,bounds.y,bounds.width,bounds.height );
		return inBounds;
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
			BasicAnchorPoint original = (BasicAnchorPoint)cxn.getOrigin();
			BasicAnchorPoint terminus = (BasicAnchorPoint)cxn.getTerminus();
			BasicAnchorPoint dominantAnchor = original;
			ConnectionType ctype = original.getConnectionType();
			if( ctype==ConnectionType.ANY ) {  // Use the the other end if it has a more specific data type
				//  try the other end
				ctype = terminus.getConnectionType();
				dominantAnchor = terminus;
			}
			
			super.hoverColor = WorkspaceConstants.CONNECTION_HOVER;
			super.selectedColor = WorkspaceConstants.CONNECTION_SELECTED;
			// Signal is different in that it has no fill
			if( ctype==ConnectionType.SIGNAL ) {    
				super.stroke = dominantAnchor.getOutlineStroke();
				super.standardColor=WorkspaceConstants.CONNECTION_FILL_SIGNAL;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			// Text is different in that it has a centerline
			else if( ctype==ConnectionType.TEXT ) {
				super.stroke = dominantAnchor.getOutlineStroke();
				super.standardColor=dominantAnchor.getOutlineColor();
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = dominantAnchor.getCoreStroke();
				super.standardColor=dominantAnchor.getCoreColor();
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = centerlineStroke;
				super.standardColor=WorkspaceConstants.CONNECTION_BACKGROUND;
				super.paintConnection(g, cxn, route, selected, hover);
			}
			else {
				super.stroke = dominantAnchor.getOutlineStroke();
				super.standardColor= dominantAnchor.getOutlineColor();
				super.paintConnection(g, cxn, route, selected, hover);
				super.stroke = original.getCoreStroke(dominantAnchor.getConnectionType());
				super.standardColor=original.getCoreColor(dominantAnchor.getConnectionType());
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
		
		// Change all stubs and downstream connections to the selected type.
		// This does NOT make the block dirty, since the changes are automatically.
		// Special case: ignore change to an incoming control port.
		// synched with the gateway.
		public void actionPerformed(ActionEvent e) {
			block.changeConnectorType(connectionType);
			List<SerializableAnchor> anchors = new ArrayList<SerializableAnchor>();
			for( AnchorDescriptor anchor:block.getAnchors()) {
				if( anchor.getDisplay().equalsIgnoreCase(BlockConstants.RECEIVER_PORT_NAME)) continue;
				anchors.add(block.convertAnchorToSerializable((ProcessAnchorDescriptor)anchor));
			}
			requestHandler.updateBlockAnchors(diagram.getResourceId(),block.getId().toString(),anchors); // update gateway. 
			diagram.updateConnectionTypes(block,connectionType);
			getActiveDiagram().fireStateChanged();
		}
	}


	public void alignLeft() {
		Collection<BlockComponent> selections = getSelectedBlocks();
		Rectangle topLeft = findTopLeft(selections);
		// Let's make sure we can undo this :-)
		HashMap<JComponent, Rectangle2D> undoMap = new HashMap<>();
		for(BlockComponent block:selections) {
			Point loc = block.getBlock().getLocation();
			undoMap.put(block,  block.getBounds());
			block.getBlock().setLocation(new Point(topLeft.x, loc.y));
		}
		UndoManager.getInstance().add(new UndoMoveBlocks(undoMap));
		getActiveDiagram().fireStateChanged();
	}
	
	public void alignRight() {
		Collection<BlockComponent> selections = getSelectedBlocks();
		Rectangle bottomRight = findBottomRight(selections);
		// Let's make sure we can undo this :-)
		HashMap<JComponent, Rectangle2D> undoMap = new HashMap<>();
		for(BlockComponent block:selections) {
			Point loc = block.getBlock().getLocation();
			undoMap.put(block,  block.getBounds());
			block.getBlock().setLocation(new Point(bottomRight.x-block.getWidth(), loc.y));
		}
		UndoManager.getInstance().add(new UndoMoveBlocks(undoMap));
		getActiveDiagram().fireStateChanged();
	}

	public void alignWidthCenter() {
		Collection<BlockComponent> selections = getSelectedBlocks();
		Rectangle bottomRight = findBottomRight(selections);
		// Let's make sure we can undo this :-)
		HashMap<JComponent, Rectangle2D> undoMap = new HashMap<>();
		// align centered so the lines look good
		for(BlockComponent block:selections) {
			Point loc = block.getBlock().getLocation();
			undoMap.put(block,  block.getBounds());
			int adjust = (bottomRight.width - block.getWidth()) / 2; 
			block.getBlock().setLocation(new Point(bottomRight.x-block.getWidth()-adjust, loc.y));
		}
		UndoManager.getInstance().add(new UndoMoveBlocks(undoMap));
		getActiveDiagram().fireStateChanged();
	}

	public void alignHeightCenter() {
		Collection<BlockComponent> selections = getSelectedBlocks();
		Rectangle topLeft = findTopLeft(selections);
		// Let's make sure we can undo this :-)
		HashMap<JComponent, Rectangle2D> undoMap = new HashMap<>();
		for(BlockComponent block:selections) {
			Point loc = block.getBlock().getLocation();
			// align centered so the lines look good
			int adjust = (topLeft.height - block.getHeight()) / 2; 
			undoMap.put(block,  block.getBounds());
			block.getBlock().setLocation(new Point(loc.x, topLeft.y + adjust));
		}
		UndoManager.getInstance().add(new UndoMoveBlocks(undoMap));
		getActiveDiagram().fireStateChanged();
	}

	public void alignTop() {
		Collection<BlockComponent> selections = getSelectedBlocks();
		Rectangle topLeft = findTopLeft(selections);
		// Let's make sure we can undo this :-)
		HashMap<JComponent, Rectangle2D> undoMap = new HashMap<>();
		for(BlockComponent block:selections) {
			Point loc = block.getBlock().getLocation();
			undoMap.put(block,  block.getBounds());
			block.getBlock().setLocation(new Point(loc.x, topLeft.y));
		}
		UndoManager.getInstance().add(new UndoMoveBlocks(undoMap));
		getActiveDiagram().fireStateChanged();
	}

	public void alignBottom() {
		Collection<BlockComponent> selections = getSelectedBlocks();
		Rectangle bottomRight = findBottomRight(selections);
		// Let's make sure we can undo this :-)
		HashMap<JComponent, Rectangle2D> undoMap = new HashMap<>();
		for(BlockComponent block:selections) {
			Point loc = block.getBlock().getLocation();
			undoMap.put(block,  block.getBounds());
			block.getBlock().setLocation(new Point(loc.x, bottomRight.y-block.getHeight()));
		}
		UndoManager.getInstance().add(new UndoMoveBlocks(undoMap));
		getActiveDiagram().fireStateChanged();
	}
	
	
	/**
	 * Trigger the propagation method of the currently selected block. 
	 * NOTE: We call it "evaluate", but we really call propagate().
	 */
	private class PropagateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public PropagateAction(ProcessBlockView blk)  {
			super(PREFIX+".Propagate",IconUtil.getIcon("window_play"));  // preferences
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			ProcessDiagramView pdv = getActiveDiagram();
			requestHandler.propagateBlockState(pdv.getResourceId(),block.getId().toString());
		}
	}
	/**
	 * Trigger the propagation method of blocks connected to the input of the currently
	 * selected block.
	 */
	private class TriggerUpstreamPropagationAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public TriggerUpstreamPropagationAction(ProcessBlockView blk)  {
			super(PREFIX+".PropagateUpstreamBlocks",IconUtil.getIcon("window_play"));  // preferences
			this.block = blk;
		}

		public void actionPerformed(ActionEvent e) {
			ProcessDiagramView pdv = getActiveDiagram();
			for(Connection cxn : pdv.getConnections()) {
				AnchorPoint ap = cxn.getTerminus();
				if( ap.getBlock().getId().equals(block.getId())) {
					Block origin = cxn.getOrigin().getBlock();
					requestHandler.propagateBlockState(pdv.getResourceId(),origin.getId().toString());
				}
			}
		}
	}
	/**
	 * Display a popup dialog for this block showing an explanation
	 * for its state.  This applies only to blocks that are currently
	 * TRUE or FALSE.
	 */
	private class ExplanationAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessDiagramView diagram;
		private final ProcessBlockView block;
		public ExplanationAction(ProcessDiagramView dia,ProcessBlockView blk)  {
			super(PREFIX+".Explanation");
			this.diagram = dia;
			this.block = blk;
		}
		
		// Display the internals viewer
		public void actionPerformed(final ActionEvent e) {
			final JDialog viewer = (JDialog)new BlockExplanationViewer(context.getFrame(),diagram,block);
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
	 * Display a dialog that allows the user to enter a value for
	 * each output -- and then force propagation of that value. 
	 */
	private class ForceAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessDiagramView diagram;
		private final ProcessBlockView block;
		public ForceAction(ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".Force");
			this.diagram = diag;
			this.block = blk;
		}
		
		// Display a dialog that allows user-entry of output values
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
	 * Display context-sensitive help in a browser window. 
	 * NOTE: On windows systems the #xxxx portion of the URL was stripped when sending to the 
	 *       default browser, so instead we use the file explorer.
	 */
	private class HelpAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		public HelpAction(ProcessBlockView blk)  {
			super(PREFIX+".Help");
			this.block = blk;
		}
		
		// Display a browser pointing to the help text for the block.
		// Somehow IA got their python to work on Windows. Our attempts kept getting the #xxx stripped off.
		// Thus we use the script instead of desktop.browse.
		public void actionPerformed(final ActionEvent e) {
			Desktop desktop=Desktop.getDesktop();
			String hostname = requestHandler.getGatewayHostname();
			String address = String.format("http:/%s:8088/main/%s#%s",hostname,BLTProperties.ROOT_HELP_PATH,block.getClassName());
			try {
				if( OS.indexOf("win")>=0) {
					String browserPath = requestHandler.getWindowsBrowserPath();
					if( browserPath==null ) browserPath = "PATH_NOT_FOUND_IN_ORM";
					log.infof("%s.HelpAction: Windows command: %s %s",CLSS,browserPath,address);
					new ProcessBuilder().command(browserPath, address).start();
				}
				else {
					URI url = new URI(address);
					log.infof("%s.HelpAction: URI is: %s",CLSS,url.toASCIIString());
					desktop.browse(url);
				}
			}
			catch(URISyntaxException use) {
				log.infof("%s.HelpAction: illegal syntax in %s (%s)",CLSS,address,use.getLocalizedMessage()); 
			}
			catch(IOException ioe) {
				log.infof("%s.HelpAction: Exception posting browser (%s)",CLSS,ioe.getLocalizedMessage()); 
			}
		}
	}

	/**
	 * Select All blocks.
	 */
	private class SelectAllBlocksAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public SelectAllBlocksAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".SelectAllBlocks",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			selectAllBlocks();
		}
	}
	
	/**
	 * Left align selected blocks.
	 */
	private class AlignLeftAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public AlignLeftAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk, List<JComponent> selections)  {
			super(PREFIX+".AlignBlocksLeft",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			alignLeft();
		}
	}
	
	/**
	 * Right align selected blocks.
	 */
	private class AlignRightAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public AlignRightAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk, List<JComponent> selections)  {
			super(PREFIX+".AlignBlocksRight",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			alignRight();
		}
	}
	
	/**
	 * Center Width align selected blocks.
	 */
	private class AlignWidthCenterAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public AlignWidthCenterAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk, List<JComponent> selections)  {
			super(PREFIX+".AlignBlocksWidthCenter",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			alignWidthCenter();
		}
	}
	
	/**
	 * center height align selected blocks.
	 */
	private class AlignHeightCenterAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public AlignHeightCenterAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk, List<JComponent> selections)  {
			super(PREFIX+".AlignBlocksHeightCenter",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			alignHeightCenter();
		}
	}
	
	/**
	 * Top align selected blocks.
	 */
	private class AlignTopAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public AlignTopAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk, List<JComponent> selections)  {
			super(PREFIX+".AlignBlocksTop",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			alignTop();
		}
	}
	
	/**
	 * Bottom align selected blocks.
	 */
	private class AlignBottomAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public AlignBottomAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk, List<JComponent> selections)  {
			super(PREFIX+".AlignBlocksBottom",null);  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			alignBottom();
		}
	}
	
	/**
	 * Place the currently selected block in lock mode.
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
		
		// TODO: lock the block in the gateway ...
		public void actionPerformed(ActionEvent e) {
			block.setLocked(true);
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
		/**
		 * Resetting a block does not affect the saved state and,
		 * therefore, does not make the diagram dirty.
		 */
		public void actionPerformed(ActionEvent e) {
			ProcessDiagramView pdv = getActiveDiagram();
			requestHandler.resetBlock(pdv.getResourceId(),block.getName());
		}
	}

	/**
	 * Display sub-menus listing the diagrams connected to the subject source
	 * or sink 
	 */
	private class ShowDiagramAction extends AbstractAction {
		private static final long serialVersionUID = 1L;
		private final SerializableResourceDescriptor diagram;
		private final String bname;
		
		public ShowDiagramAction(SerializableResourceDescriptor desc,String blockName)  {
			super(desc.getName());
			this.diagram = desc;
			this.bname = blockName;
		}
		
		// List paths to all connected diagrams
		public void actionPerformed(ActionEvent e) {
			String path = requestHandler.pathForBlock(diagram.getResourceId(), bname);
			int pos = path.lastIndexOf(":");    // Strip off block name
			if( pos>0 ) path = path.substring(0, pos);
			ProjectBrowserRoot project = context.getProjectBrowserRoot();
        	AbstractNavTreeNode root = null;
        	AbstractNavTreeNode node = null;
        	root = project.findChild("Project");
        	if(root!=null) node = findChildInTree(root,"ROOT");
        	// The specified path is colon-delimited.
			String[] pathArray = path.toString().split(":");

			int index = 1;  // Skip the leading colon
			while( index<pathArray.length ) {
				node = findChildInTree(node,pathArray[index]);
				if( node!=null ) {
					node.expand();
					try {
						Thread.sleep(100); 
					}
					catch(InterruptedException ignore) {}
				}
				else{
					log.warnf("%s.receiveNotification: Unable to find node (%s) on browser path",CLSS,pathArray[index]);
					break;
				}
				index++;
			}

			if( node!=null ) {
				node.onDoubleClick();    // Opens the diagram
			}
			else {
				log.warnf("%s.receiveNotification: Unable to open browser path (%s)",CLSS,path.toString());
			}
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}

		/**
		 * We have not been successful with the findChild method .. so we've taken it on ourselves.
		 * @param root
		 * @param name
		 * @return
		 */
		private AbstractNavTreeNode findChildInTree(AbstractNavTreeNode root,String name) {
			AbstractNavTreeNode match = null;
			if( root!=null ) {
				@SuppressWarnings("unchecked")
				Enumeration<AbstractNavTreeNode> nodeWalker = root.children();
				AbstractNavTreeNode child = null;
				
				while( nodeWalker.hasMoreElements() ) {
					child = nodeWalker.nextElement();
					log.infof("%s.findChildInTree: testing %s vs %s",CLSS,name,child.getName());
					if( child.getName().equalsIgnoreCase(name)) {
						match = child;
						break;
					}
				}
			}
			return match;
		}
	}

	/**
	 * Configure the block to show the generic signal connection stub.
	 * This option is not available once the diagram has been saved
	 */
	private class HideSignalAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final ProcessBlockView block;
		private final ProcessDiagramView diagram;
		private final DiagramWorkspace workspace;
		public HideSignalAction(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView blk)  {
			super(PREFIX+".HideSignal",IconUtil.getIcon("trafficlight_red"));  // preferences
			this.workspace = wksp;
			this.diagram = diag;
			this.block = blk;
		}

		// TO DO : set value in gateway?
		public void actionPerformed(ActionEvent e) {
			block.setSignalAnchorDisplayed(false);
			block.fireStateChanged();
			// Repaint to update the stub
			SwingUtilities.invokeLater(new WorkspaceRepainter());
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

		// TO: Set diagram dirty ...
		public void actionPerformed(ActionEvent e) {
			block.setSignalAnchorDisplayed(true);
			block.fireStateChanged();
			// Repaint to update the stub
			SwingUtilities.invokeLater(new WorkspaceRepainter());
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

		// TO DO: Set diagram dirty.
		public void actionPerformed(ActionEvent e) {
			block.setLocked(false);
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
			super(PREFIX+".ViewInternals",IconUtil.getIcon("sun"));
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

	private JComboBox<String> createZoomDropDown() {
        JComboBox<String> combo = new JComboBox<String>();
        combo.addItem("25%");
        combo.addItem("50%");
        combo.addItem("75%");
        combo.addItem("100%");
        combo.addItem("200%");
        combo.setSelectedIndex(3);
        return combo;
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
    /**
     * Use this class to position a block (e.g. AttributeDisplay).
     * Position the specified block, then repaint.
     *
     */
    public class BlockPositioner implements Runnable {
    	private final DiagramWorkspace workspace;
    	private final BlockAttributeView block;
    	private final Point location;
    			
    	public BlockPositioner(DiagramWorkspace wksp,BlockAttributeView blk,Point loc) {
    		this.workspace = wksp;
    		this.block = blk;
    		this.location = loc;
    	}
    		
    	@Override
    	public void run() {
    		block.setLocation(location);
    		block.blockMoved(block);
    		workspace.repaint(200);  // Paint in 200 ms
    	}
    }
    
	// ============================== Change Listener ================================
	/**
	 * If the current diagram changes (e.g. block dropped on it), then paint the background accordingly.
	 * This is triggered by a fireStateChange call to the diagram.
	 *
	 * @param event
	 */
	@Override
	public void stateChanged(ChangeEvent event) {
		///log.infof("%s.stateChanged: source = %s",CLSS,event.getSource().getClass().getCanonicalName());
		if( event.getSource() instanceof ProcessDiagramView ) {
			ProcessDiagramView diagram = (ProcessDiagramView)event.getSource();
			statusManager.setPendingView(diagram.getResourceId(), diagram);
		}
		updateBackgroundForDiagramState();
	}
   
}
