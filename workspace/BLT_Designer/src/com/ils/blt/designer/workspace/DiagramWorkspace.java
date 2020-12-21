/**
 *   (c) 2014-2020  ILS Automation. All rights reserved.
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
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import javax.swing.AbstractAction;
import javax.swing.Action;
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
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.block.Input;
import com.ils.block.Output;
import com.ils.block.SinkConnection;
import com.ils.block.SourceConnection;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.script.CommonScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.config.BlockExplanationViewer;
import com.ils.blt.designer.config.BlockInternalsViewer;
import com.ils.blt.designer.config.BlockPropertiesSelector;
import com.ils.blt.designer.config.ForceValueSettingsDialog;
import com.ils.blt.designer.editor.BlockEditConstants;
import com.ils.blt.designer.editor.BlockPropertyEditor;
import com.ils.blt.designer.editor.PropertyEditorFrame;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.sqltags.ClientTagManager;
import com.inductiveautomation.ignition.client.sqltags.tree.TagPathTreeNode;
import com.inductiveautomation.ignition.client.sqltags.tree.TagTreeNode;
import com.inductiveautomation.ignition.client.util.LocalObjectTransferable;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.config.ObservablePropertySet;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectScope;
import com.inductiveautomation.ignition.common.sqltags.model.Tag;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.TagProp;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
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
import com.inductiveautomation.ignition.designer.sqltags.tree.dnd.NodeListTransferable;
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
							  			ChangeListener                                   {
	private static final String ALIGN_MENU_TEXT = "Align Blocks";
	private static final String CLSS = "DiagramWorkspace";
	private static final long serialVersionUID = 4627016159409031941L;
	private static final DataFlavor BlockDataFlavor = LocalObjectTransferable.flavorForClass(ObservablePropertySet.class);
	public static final String key = "BlockDiagramWorkspace";
	private static String OS = System.getProperty("os.name").toLowerCase();
	public static final String PREFIX = BLTProperties.BLOCK_PREFIX; 
	private final ApplicationRequestHandler handler = new ApplicationRequestHandler();
	private final DesignerContext context;
	private final EditActionHandler editActionHandler;
	private final ExecutionManager executionEngine;
	private final NodeStatusManager statusManager;
	private Collection<ResourceWorkspaceFrame> frames;
	private LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	
	private PopupListener rightClickHandler;
	private JPopupMenu zoomPopup;
	private JComboBox<String> zoomCombo;
	private CommandBar alignBar = null;
	private ApplicationRequestHandler  requestHandler;

	/**
	 * Constructor:
	 */
	public DiagramWorkspace(DesignerContext ctx) {
		super(ProjectScope.PROJECT);
		this.context = ctx;
		this.editActionHandler = new BlockActionHandler(this,context);
		this.executionEngine = new BasicExecutionEngine(1,CLSS);
		this.addDesignableWorkspaceListener(this);
		this.zoomPopup = createZoomPopup();
		this.rightClickHandler = new PopupListener();
		this.addMouseListener(rightClickHandler);
		requestHandler = new ApplicationRequestHandler();
		 //		this.keyHandler = new KeystrokeListener();
//		this.addKeyListener(keyHandler);

		// none of this keystroke stuff works
	    KeyStroke ltA = KeyStroke.getKeyStroke("a");
	    Action lt_A_Action = new AbstractAction(){
	      public void actionPerformed(ActionEvent e){
	    	  logger.errorf("DiagramWorkspace keyReleased A");
	    	  selectAllBlocks();
	      }
	    };
//	    this.getInputMap().put(ltA, "LT_A");
	    this.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ltA, "LT_A"); 
	    this.getActionMap().put("LT_A", lt_A_Action);


	    
	    KeyStroke altA = KeyStroke.getKeyStroke(KeyEvent.VK_A,java.awt.event.InputEvent.ALT_DOWN_MASK,false);
	    Action alt_A_Action = new AbstractAction(){
	      public void actionPerformed(ActionEvent e){
	    	  selectAllBlocks();
	      }
	    };
	    this.getInputMap().put(altA, "ALT_A");
//	    this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(altA, "ALT_A");
	    this.getActionMap().put("ALT_A", alt_A_Action);

		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		initialize();
		setBackground(Color.red);
		this.requestHandler = new ApplicationRequestHandler();
	}


	// Initialize the workspace frames.
	private void initialize() {
		// Create palette
		ProcessBlockPalette tabbedPalette = new ProcessBlockPalette(context, this);
		tabbedPalette.setInitMode(DockContext.STATE_FRAMEDOCKED);
		tabbedPalette.setInitSide(DockContext.DOCK_SIDE_NORTH);
		tabbedPalette.setInitIndex(0);
		tabbedPalette.setTitle(BundleUtil.get().getString(PREFIX+".Palette.Title"));
		tabbedPalette.setTabTitle(BundleUtil.get().getString(PREFIX+".Palette.Tab.Title"));
		tabbedPalette.setSideTitle("SideTitle");
		tabbedPalette.putClientProperty("menu.text", BundleUtil.get().getString(PREFIX+".Palette.Title"));
		
		frames = new ArrayList<ResourceWorkspaceFrame>();
		frames.add(tabbedPalette);
		
		PropertyEditorFrame pef = new PropertyEditorFrame(context, this);
		pef.setInitMode(DockContext.STATE_FRAMEDOCKED);
		pef.setInitSide(DockContext.DOCK_SIDE_WEST);
		pef.setInitIndex(10);
		pef.putClientProperty("menu.text", "Symbolic AI Property Editor");
		frames.add(pef);
		
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
	 * For popup menu
	 */
	@Override
	public JPopupMenu getSelectionPopupMenu(List<JComponent> selections) {
		if( selections.size()>0 ) {
			JComponent selection = selections.get(0);
			logger.debugf("%s.getSelectionPopupMenu: Component is: %s",CLSS,selections.get(0).getClass().getName());
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
						!pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT) ) {
					
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
						logger.debugf("%s.getSelectionPopupMenu: Connection type is: %s",CLSS,ct.name());
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
					logger.infof("%s.getSelectionPopupMenu: SINK",CLSS);
					JMenu linkSinkMenu = new JMenu(BundleUtil.get().getString(PREFIX+".FollowConnection.Name"));
					linkSinkMenu.setToolTipText(BundleUtil.get().getString(PREFIX+".FollowConnection.Desc"));
					String diagramId = getActiveDiagram().getId().toString();
					List<SerializableBlockStateDescriptor> descriptors = handler.listSourcesForSink(diagramId, pbv.getId().toString());
					for(SerializableBlockStateDescriptor desc:descriptors) {
						SerializableResourceDescriptor rd = handler.getDiagramForBlock(desc.getIdString());
						if( rd==null ) continue;
						ShowDiagramAction action = new ShowDiagramAction(rd,desc.getName());
						linkSinkMenu.add(action);
					}
					menu.add(linkSinkMenu);
				}
				else if( pbv.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE) )  {
					logger.infof("%s.getSelectionPopupMenu: SOURCE",CLSS);
					JMenu linkSourceMenu = new JMenu(BundleUtil.get().getString(PREFIX+".FollowConnection.Name"));
					linkSourceMenu.setToolTipText(BundleUtil.get().getString(PREFIX+".FollowConnection.Desc"));
					
					String diagramId = getActiveDiagram().getId().toString();
					List<SerializableBlockStateDescriptor> descriptors = handler.listSinksForSource(diagramId, pbv.getId().toString());
					for(SerializableBlockStateDescriptor desc:descriptors) {
						SerializableResourceDescriptor rd = handler.getDiagramForBlock(desc.getIdString());
						if( rd==null ) continue;
						ShowDiagramAction action = new ShowDiagramAction(rd,desc.getName());
						linkSourceMenu.add(action);
					}
					menu.add(linkSourceMenu);
				}
				logger.debugf("%s.getSelectionPopupMenu: Selection editor class = %s",CLSS,pbv.getEditorClass());
				// Do not allow editing when the diagram is disabled
				if(pbv.getEditorClass() !=null && pbv.getEditorClass().length() > 0 &&
						!getActiveDiagram().getState().equals(DiagramState.DISABLED)) {
					CustomEditAction cea = new CustomEditAction(this,pbv);
					menu.add(cea);
				}
				if(!getActiveDiagram().getState().equals(DiagramState.DISABLED)) {
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
				
				String currentState = handler.getBlockState(getActiveDiagram().getId().toString(),pbv.getName());
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
				menu.add(context.getDeleteAction());
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

    	logger.tracef("EREIAM JH - GetMenu, context: %s",""+getContext());
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
			final JDialog viewer = (JDialog)new BlockPropertiesSelector(context.getFrame(),diagram,block, workspace);
			
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
	
	
	@Override
	public boolean handleDrop(Object droppedOn,DropTargetDropEvent event) {
		if (droppedOn instanceof BlockComponent) {
			handleBlockDrop(droppedOn, event);
		}
		if (droppedOn instanceof DiagramWorkspace) {
			handleDiagramDrop(droppedOn, event);
		}
		if (event.isDataFlavorSupported(BlockDataFlavor)) {
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
						// Null doesn't work here ...
						
						logger.infof("%s.handleDrop: dropped %s",CLSS,event.getTransferable().getTransferData(BlockDataFlavor).getClass().getName());
						return true;
					}
					else {
						logger.infof("%s.handleDrop: drop of %s out-of-bounds",CLSS,event.getTransferable().getTransferData(BlockDataFlavor).getClass().getName());
					}
				}
				else {
					logger.infof("%s.handlerDrop: Unexpected class (%s),  rejected",
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
	private void handleDiagramDrop(Object droppedOn, DropTargetDropEvent event) {
		DataFlavor flava = NodeListTransferable.FLAVOR_NODELIST;
		if (event.isDataFlavorSupported(flava)) {
			try {
				Object node = event.getTransferable().getTransferData(flava);
				if (node instanceof ArrayList && ((ArrayList) node).size() == 1) {
					ArrayList<?> tagNodeArr = (ArrayList<?>)node;
					// NOTE: This will fail (properly) if tag type is a dataset
					if (tagNodeArr.get(0) instanceof TagTreeNode) {  // That's the thing we want!
						BlockDesignableContainer container = getSelectedContainer();
						ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
						TagTreeNode tnode = (TagTreeNode) tagNodeArr.get(0);
						int dropx = event.getLocation().x;
						int thewidth = getActiveDiagram().getDiagramSize().width;
						nameFromTagTree(tnode);

						if( getSelectedContainer()!=null ) {
							ProcessBlockView block = null;
							TagPath tp = tnode.getTagPath();
							boolean isStandardFolder = BusinessRules.isStandardConnectionsFolder(tp);
							// Sinks to right,sources to the left
							if (dropx < thewidth / 2) {
								if( isStandardFolder ) {
									SourceConnection source = new SourceConnection();
									block = new ProcessBlockView(source.getBlockPrototype().getBlockDescriptor());
									block.setName(nameFromTagTree(tnode));
								}
								else {
									Input input = new Input();
									block = new ProcessBlockView(input.getBlockPrototype().getBlockDescriptor());
									block.setName(enforceUniqueName(nameFromTagTree(tnode),diagram));
								}
							} 
							else {
								if( isStandardFolder ) {
									SinkConnection sink = new SinkConnection();
									block = new ProcessBlockView(sink.getBlockPrototype().getBlockDescriptor());
									block.setName(nameFromTagTree(tnode));
								}
								else {
									Output output = new Output();
									block = new ProcessBlockView(output.getBlockPrototype().getBlockDescriptor());
									block.setName(enforceUniqueName(nameFromTagTree(tnode),diagram));
								}
							}
							
							DesignPanel panel = getSelectedDesignPanel();
							BlockDesignableContainer bdc = (BlockDesignableContainer) panel.getDesignable();
							Point dropPoint = SwingUtilities.convertPoint(
									event.getDropTargetContext().getComponent(),
									panel.unzoom(event.getLocation()), bdc);
							this.setCurrentTool(getSelectionTool());   // So the next select on workspace does not result in another block

							if( isInBounds(dropPoint,bdc) ) {
								block.setLocation(dropPoint);
								this.getActiveDiagram().addBlock(block);
								DataType type = null;
								ClientTagManager tmgr = context.getTagManager();
								Tag tag = tmgr.getTag(tnode.getTagPath());
								type = tag.getDataType();
								Collection<BlockProperty> props = block.getProperties();
								for (BlockProperty property:props) {
									if( BlockConstants.BLOCK_PROPERTY_TAG_PATH.equalsIgnoreCase(property.getName())) {
										property.setBinding(tnode.getTagPath().toStringFull());}
										block.modifyConnectionForTagChange(property, type);
								}
								logger.infof("%s.handleDrop: dropped %s",CLSS,block.getClass().getName());
							}
							else {
								logger.infof("%s.handleDrop: drop of %s out-of-bounds",CLSS,block.getClass().getName());
							}
						}
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
	public void handleBlockDrop(Object droppedOn, DropTargetDropEvent event) {
		try {
			DataFlavor flava = NodeListTransferable.FLAVOR_NODELIST;
			if (event.isDataFlavorSupported(flava)) {
				Object node = event.getTransferable().getTransferData(flava);
				if (node instanceof ArrayList && ((ArrayList) node).size() == 1) {
					ArrayList<?> tagNodeArr = (ArrayList<?>)node;
					if (tagNodeArr.get(0) instanceof TagTreeNode) {  // That's the thing we want!
						BlockDesignableContainer container = getSelectedContainer();
						ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
						TagTreeNode tnode = (TagTreeNode) tagNodeArr.get(0);
						logger.infof("%s.handleDrop: tag data: %s",CLSS,tnode.getName());
						TagPath tp = tnode.getTagPath();

						Block targetBlock = ((BlockComponent)droppedOn).getBlock();
						if(targetBlock instanceof ProcessBlockView) {
							ProcessBlockView pblock = (ProcessBlockView)targetBlock;
							BlockProperty prop = pblock.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
							if( prop==null) return;  // Unless there's a tag path, do nothing
							
							DataType tagType = null;
							ClientTagManager tmgr = context.getTagManager();
							Tag tag = tmgr.getTag(tnode.getTagPath());
							tagType = tag.getDataType();
							Integer tagProp = (Integer)tag.getAttribute(TagProp.ExpressionType).getValue();
							String connectionMessage = diagram.isValidBindingChange(pblock, prop, tp.toStringFull(), tagType,tagProp);
							
							if( connectionMessage==null ) {
								prop.setBinding(tnode.getTagPath().toStringFull());
								setSelectedItems((JComponent)null);  // hack to get the property panel to refresh
								setSelectedItems((JComponent)droppedOn);
								pblock.setName(nameFromTagTree(tnode));
								pblock.setCtypeEditable(true);
								pblock.modifyConnectionForTagChange(prop, tagType);
								saveOpenDiagram(diagram.getResourceId());
								diagram.fireStateChanged();
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
			logger.error(String.format("%s.handleDrop: Exceptiona: %s",CLSS,ex.getLocalizedMessage()),ex);
		}
	}

	@Override
	public void onActivation() {
		zoomCombo.setVisible(true);
		logger.infof("%s: onActivation",CLSS);
		
	}


	@Override
	public void onDeactivation() {
		zoomCombo.setVisible(false);
		logger.infof("%s: onDeactivation",CLSS);
	}
	
	// Guarantee a unique name for a block that has not yet been added to the diagram.
	private String enforceUniqueName(String name,ProcessDiagramView diagram) {
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
	
	private String nameFromTagTree(TagTreeNode tnode) {
		String name = tnode.getName();
		while(tnode.inUDTInstance()) {
			TagPathTreeNode tptn = tnode.getParent();
			if( tptn instanceof TagTreeNode) {
				tnode = (TagTreeNode)tptn;
				name = tnode.getName();
			}
			else {
				name = tptn.getName();
				break;
			}
		}
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
		logger.infof("%s: copyBlocks",CLSS);
		ObjectMapper mapper = new ObjectMapper();
		String json = null;
		List<SerializableBlock> list = new ArrayList<SerializableBlock>();
		for( Block blk:blocks) {
			logger.infof("%s: copyBlocks class=%s",CLSS,blk.getClass().getName());
			ProcessBlockView view = (ProcessBlockView)blk;
			SerializableBlock sb = view.convertToSerializable();
			list.add(sb);
		}
		try{ 
			   json = mapper.writeValueAsString(list);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize block list (%s)",CLSS,jpe.getMessage());
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
		logger.infof("%s.pasteBlocks: %s",CLSS,json);
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
					saveAuxDataProduction(pbv, theDiagram.getId().toString());
				} else {
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
						logger.infof("%s: new diagram for encapsulation block ...",CLSS);
						try{ 
						    json = mapper.writeValueAsString(diagram);
						}
						catch(JsonProcessingException jpe) {
							logger.warnf("%s: Unable to serialize diagram (%s)",CLSS,jpe.getMessage());
						}
						logger.infof("%s: serializeDiagram created json ... %s",CLSS,json);

						byte[] bytes = json.getBytes();
						logger.debugf("%s: DiagramAction. create new %s resource %d (%d bytes)",CLSS,BLTProperties.DIAGRAM_RESOURCE_TYPE,
								newId,bytes.length);
						ProjectResource resource = new ProjectResource(newId,
								BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
								pbv.getName(), ApplicationScope.GATEWAY, bytes);
						resource.setParentUuid(getActiveDiagram().getId());
						executionEngine.executeOnce(new ResourceUpdateManager(this,resource));					
					} 
					catch (Exception err) {
						ErrorUtil.showError(CLSS+" Exception pasting blocks",err);
					}
				}
			}
		} 
		catch (JsonParseException jpe) {
			logger.warnf("%s: pasteBlocks parse exception (%s)",CLSS,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			logger.warnf("%s: pasteBlocks mapping exception (%s)",CLSS,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			logger.warnf("%s: pasteBlocks IO exception (%s)",CLSS,ioe.getLocalizedMessage());
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
	
	public void open (long resourceId) {
		logger.debugf("%s: open - already open (%s)",CLSS,(isOpen(resourceId)?"true":"false"));
		if(isOpen(resourceId) ) {
			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId);
			open(tab);  // Brings tab to front
		}
		else {
			ProjectResource res = context.getProject().getResource(resourceId);	
			String json = new String(res.getData());
			logger.debugf("%s: open - diagram = %s",CLSS,json);
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
				// Synchronize names as the resource may have been re-named and/or
				// state changed since it was serialized
				sd.setName(res.getName());
				sd.setState(statusManager.getResourceState(resourceId));
			} 
			catch (JsonParseException jpe) {
				logger.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				logger.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				logger.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
			}
			ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
			for( Block blk:diagram.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				diagram.initBlockProperties(pbv);
			}
			
			super.open(diagram);
			saveOpenDiagram(resourceId);
			// Inform the gateway of the state and let listeners update the UI
			ApplicationRequestHandler arh = new ApplicationRequestHandler();
			arh.setDiagramState(diagram.getId().toString(), diagram.getState().name());
			statusManager.setResourceState(resourceId,diagram.getState(),true);
			diagram.setDirty(false);  // Newly opened from a serialized resource, should be in-sync.
			// In the probable case that the designer is opened after the diagram has started
			// running in the gateway, obtain any updates
			diagram.refresh();
			
			BlockDesignableContainer tab = (BlockDesignableContainer)findDesignableContainer(resourceId);
			tab.setBackground(diagram.getBackgroundColorForState());
			
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	public void close (long resourceId) {
		logger.infof("%s: close resource %d",CLSS,resourceId);
		super.close(findDesignableContainer(resourceId));
	}
	
	// On close we can save the container, no questions asked with a
	// call to: saveDiagram((BlockDesignableContainer)container);
	// As it is ... a dialog pops up.
	@Override
	protected void onClose(DesignableContainer c) {
		logger.debugf("%s: onClose",CLSS);
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
					logger.warnf("%s.onClose: serialization exception (%s)",CLSS, jpe.getLocalizedMessage());
				}
			}
			else {
				// Mark diagram as clean, since we reverted changes
				diagram.setDirty(false);
			}
		}
		DiagramTreeNode node = (DiagramTreeNode)statusManager.findNode(container.getResourceId());
		if( node!=null ) {
			node.setIcon(node.getIcon());
			node.refresh();
		}
		diagram.unregisterChangeListeners();
		
	}
	/**
	 * This is called as a result of a user "Save" selection on
	 * the main menu. If the diagram indicated by the resourceId
	 * is open, then save it.
	 */
	public void saveOpenDiagram(long resourceId) {
		logger.debugf("%s: saveOpenDiagrams",CLSS);
		for(DesignableContainer dc:openContainers.keySet()) {
			BlockDesignableContainer bdc = (BlockDesignableContainer)dc;
			if( bdc.getResourceId()==resourceId ) {
				saveDiagramResource((BlockDesignableContainer)dc);
			}
		}
	}
	
	/**
	 * This method obtains the project resourceId from the container and then 
	 * saves the project resource. We assume that the resource has been updated.
	 * @param c the tab
	 */
	public void saveDiagramResource(BlockDesignableContainer c) {
		ProcessDiagramView diagram = (ProcessDiagramView)c.getModel();
		logger.infof("%s.saveDiagramResource - %s ...",CLSS,diagram.getDiagramName());
		diagram.registerChangeListeners();     // The diagram may include new components
		long resid = diagram.getResourceId();
		executionEngine.executeOnce(new ResourceUpdateManager(this,context.getProject().getResource(resid)));
		
		diagram.setDirty(false);
		c.setBackground(diagram.getBackgroundColorForState());
		SwingUtilities.invokeLater(new WorkspaceRepainter());
	}
	
	/**
	 * We've made a major change on the currently active diagram. Set its background accordingly.
	 * The diagram should have set its own state.
	 */
	private void updateBackgroundForDirty() {
		BlockDesignableContainer container = getSelectedContainer();
		if( container!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)(container.getModel());		
			// update any open property panels
			for (ResourceWorkspaceFrame frame: frames) {
				if (frame instanceof PropertyEditorFrame) {
					BlockPropertyEditor eddy = ((PropertyEditorFrame)frame).getEditor();
					if (eddy != null) {
						ProcessBlockView pbv = eddy.getBlock();
						if (pbv.getClassName().toLowerCase().contains("finaldiagnosis")) {  // horrible hack, but it needs a refresh
							((PropertyEditorFrame)frame).updateForFinalDiagnosis();
						}	
					}
				}
			}

			container.setBackground(view.getBackgroundColorForState());
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	// =========================== DesignableWorkspaceListener ===========================
	@Override
	public void containerClosed(DesignableContainer c) {
		logger.debugf("%s.containerClosed: %s",CLSS,c.getName());
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
		logger.debugf("%s.containerOpened: %s",CLSS,c.getName());
		BlockDesignableContainer container = (BlockDesignableContainer)c;
		ProcessDiagramView view = (ProcessDiagramView)(container.getModel());
		view.addChangeListener(this);
	}
	@Override
	public void containerSelected(DesignableContainer container) {
		if( container==null ) logger.infof("%s.containerSelected is null",CLSS);
		else logger.debugf("%s.containerSelected: %s",CLSS,container.getName());
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
				logger.debugf("%s.itemSelectionChanged: selected a %s",CLSS,selection.getClass().getName());
			} else {
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
//		CommandBar cb = dh.getAlignBar();
		CommandBar cb = getAlignBar();
		for (Component cp:cb.getComponents()) {
			cp.setEnabled(enableAlign);
		}
	}

	private boolean isInBounds(Point dropPoint,BlockDesignableContainer bdc) {
		Rectangle bounds = bdc.getBounds();
		boolean inBounds = true;
		if( dropPoint.x<bounds.x      ||
			dropPoint.y<bounds.y	  ||
			dropPoint.x>bounds.x+bounds.width ||
			dropPoint.y>bounds.y+bounds.height   )  inBounds = false;
		logger.infof("%s.handlerDrop: drop x,y = (%d,%d), bounds %d,%d,%d,%d",CLSS,dropPoint.x,dropPoint.y,bounds.x,bounds.y,bounds.width,bounds.height );
		return inBounds;
	}

	// ============================== Change Listener ================================
	/**
	 * If the current diagram changes state, then paint the background accordingly.
	 * 
	 * @param event
	 */
	@Override
	public void stateChanged(ChangeEvent event) {
		updateBackgroundForDirty();
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
			handler.updateBlockAnchors(diagram.getId(),block.getId(),anchors); // update gateway. 
			diagram.updateConnectionTypes(block,connectionType);
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
			setDirty();
			
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
		private final DiagramWorkspace workspace;
		public CustomEditAction(DiagramWorkspace wksp,ProcessBlockView blk)  {
			super(PREFIX+".ConfigureProperties");
			this.workspace = wksp;
			this.block = blk;
		}
		
		// Display the custom editor
		public void actionPerformed(final ActionEvent e) {
			// Apparently this only works if the class is in the same package (??)
			try{
				Class<?> clss = Class.forName(block.getEditorClass());
				Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {DiagramWorkspace.class,ProcessDiagramView.class,ProcessBlockView.class});
				ProcessDiagramView pdv = getActiveDiagram();
				final JDialog edtr = (JDialog)ctor.newInstance(workspace,pdv,block); 
				Object source = e.getSource();
				if( source instanceof Component) {
					edtr.setLocationRelativeTo((Component)source);
				}
				edtr.pack();
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						if( source instanceof Component) {
							edtr.setLocationRelativeTo((Component)source);
						} else {
							edtr.setLocation(100,100);
						}
						edtr.setVisible(true);
					}
				}); 
			}
			catch(InvocationTargetException ite ) {
				logger.info(CLSS+".customEditAction: Invocation failed for "+block.getEditorClass(),ite); 
			}
			catch(NoSuchMethodException nsme ) {
				logger.info(CLSS+".customEditAction: Constructor taking diagram and block not found for "+block.getEditorClass(),nsme); 
			}
			catch(ClassNotFoundException cnfe) {
				logger.info(CLSS+".customEditAction: Custom editor class "+block.getEditorClass()+" not found",cnfe);
			}
			catch( InstantiationException ie ) {
				logger.info(CLSS+".customEditAction: Error instantiating "+block.getEditorClass(),ie); 
			}
			catch( IllegalAccessException iae ) {
				logger.info(CLSS+".customEditAction: Security exception creating "+block.getEditorClass(),iae); 
			}
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
		SwingUtilities.invokeLater(new WorkspaceRepainter());

	}


	private void setDirty() {
		BlockDesignableContainer container = getSelectedContainer();
		if (container != null) {
			ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
			if (diagram != null) {
				diagram.setDirty(true);
			}
		}
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
		SwingUtilities.invokeLater(new WorkspaceRepainter());
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
		SwingUtilities.invokeLater(new WorkspaceRepainter());
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
		SwingUtilities.invokeLater(new WorkspaceRepainter());
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
		SwingUtilities.invokeLater(new WorkspaceRepainter());
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
		SwingUtilities.invokeLater(new WorkspaceRepainter());
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
			handler.propagateBlockState(pdv.getId().toString(),block.getId().toString());
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
					handler.propagateBlockState(pdv.getId().toString(),origin.getId().toString());
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
		
		// Display a browser pointing to the help text for the block
		public void actionPerformed(final ActionEvent e) {
			Desktop desktop=Desktop.getDesktop();
			String hostname = handler.getGatewayHostname();
			String address = String.format("http:/%s:8088/main/%s#%s",hostname,BLTProperties.ROOT_HELP_PATH,block.getClassName());
			try {
				if( OS.indexOf("win")>=0) {
					logger.infof("%s.HelpAction: Windows address is: %s",CLSS,address);
					Runtime.getRuntime().exec(new String[] {"explorer.exe",address} );
				}
				else {
					URI url = new URI(address);
					logger.infof("%s.HelpAction: URI is: %s",CLSS,url.toASCIIString());
					desktop.browse(url);
				}
			}
			catch(URISyntaxException use) {
				logger.infof("%s.HelpAction: Illegal URI: %s (%s)",CLSS,address,use.getLocalizedMessage()); 
			}
			catch(IOException ioe) {
				logger.infof("%s.HelpAction: Exception posting browser (%s)",CLSS,ioe.getLocalizedMessage()); 
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
		/**
		 * Block name is unique within a diagram
		 */
		public void actionPerformed(ActionEvent e) {
			ProcessDiagramView pdv = getActiveDiagram();
			handler.resetBlock(pdv.getId().toString(),block.getName());
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
			String path = handler.pathForBlock(diagram.getId(), bname);
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
					logger.warnf("%s.receiveNotification: Unable to find node (%s) on browser path",CLSS,pathArray[index]);
					break;
				}
				index++;
			}

			if( node!=null ) {
				node.onDoubleClick();    // Opens the diagram
			}
			else {
				logger.warnf("%s.receiveNotification: Unable to open browser path (%s)",CLSS,path.toString());
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
					logger.infof("%s.findChildInTree: testing %s vs %s",CLSS,name,child.getName());
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

		public void actionPerformed(ActionEvent e) {
			block.setSignalAnchorDisplayed(false);
			if( !diagram.isDirty()) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
				if( tab!=null ) workspace.saveDiagramResource(tab);
			}
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

		public void actionPerformed(ActionEvent e) {
			block.setSignalAnchorDisplayed(true);
			if( !diagram.isDirty()) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
				if( tab!=null ) workspace.saveDiagramResource(tab);
			}
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
    
    private class KeystrokeListener implements KeyListener {

        @Override
        public void keyTyped(KeyEvent e) {
			logger.errorf("DiagramWorkspace keyTyped :%s: Modifiers :%s:",e.getKeyChar(), e.getModifiers());
			System.out.println("EREIAM J = 1");
        }

        @Override
        public void keyPressed(KeyEvent e) {
			System.out.println("EREIAM J = 2");
			logger.errorf("DiagramWorkspace keyPressed :%s: Modifiers :%s:",e.getKeyChar(), e.getModifiers());
            if ((e.getKeyCode() == KeyEvent.VK_A) && ((e.getModifiers() & KeyEvent.CTRL_MASK) != 0)) {
                selectAllBlocks();
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {
			System.out.println("EREIAM J = 3");
			logger.errorf("DiagramWorkspace keyReleased :%s: Modifiers :%s:",e.getKeyChar(), e.getModifiers());
        }
    }
    

	public void copyAuxData(ProcessBlockView pbv, String oldName, String newParentId, String oldParentId) {
		
		String prodDb = requestHandler.getProductionDatabase();
		String isoDb = requestHandler.getIsolationDatabase();
//		ProcessDiagramView diagram = getActiveDiagram();
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
		
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		extensionManager.runScript(context.getScriptManager(), pbv.getClassName(), ScriptConstants.PROPERTY_GET_SCRIPT, 
				oldParentId,auxData,prodDb);

		auxData.getProperties().put("Name", pbv.getName());   // Set new key
		
		extensionManager.runScript(context.getScriptManager(), pbv.getClassName(), ScriptConstants.PROPERTY_SET_SCRIPT, 
				pbv.getId().toString(),auxData,prodDb);

		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
	
		extensionManager.runScript(context.getScriptManager(), pbv.getClassName(), ScriptConstants.PROPERTY_GET_SCRIPT, 
				oldParentId,auxData,isoDb);

		auxData.getProperties().put("Name", pbv.getName());   // Set new key

		extensionManager.runScript(context.getScriptManager(), pbv.getClassName(), ScriptConstants.PROPERTY_SET_SCRIPT, 
				pbv.getId().toString(),auxData,isoDb);
		
		// set it to the right data if not isolated  
		if( pbv.getState().equals(DiagramState.ACTIVE)) { 
			extensionManager.runScript(context.getScriptManager(), pbv.getClassName(), ScriptConstants.PROPERTY_GET_SCRIPT, 
					newParentId,auxData,prodDb);
		}
		
		if( auxData.containsData()) {
			pbv.setAuxiliaryData(auxData);
		}
		
	}

	public void saveAuxDataProduction(ProcessBlockView pbv, String parentId) {
		
		String prodDb = requestHandler.getProductionDatabase();
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		
		extensionManager.runScript(context.getScriptManager(), pbv.getClassName(), ScriptConstants.PROPERTY_SET_SCRIPT, 
				parentId,pbv.getAuxiliaryData(),prodDb);
	}

	public void saveAuxDataSbProduction(SerializableBlock sb, String parentId) {
		
		String prodDb = requestHandler.getProductionDatabase();
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		
		extensionManager.runScript(context.getScriptManager(), sb.getClassName(), ScriptConstants.PROPERTY_SET_SCRIPT, 
				parentId,sb.getAuxiliaryData(),prodDb);
	}

	public void copyApplicationAuxData(SerializableApplication sa, String newName, String oldName) {
		
		String prodDb = requestHandler.getProductionDatabase();
		String isoDb = requestHandler.getIsolationDatabase();
//		ProcessDiagramView diagram = getActiveDiagram();
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();

		
//		So, the big question is how to get the right UUID for the getapplication function to find it.
		

		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
	
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sa.getId().toString(),auxData,isoDb);

		auxData.getProperties().put("Name", newName);   // Set new key

		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
				sa.getId().toString(),auxData,isoDb);
		
		
		auxData = new GeneralPurposeDataContainer();
		
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sa.getId().toString(),auxData,prodDb);

		auxData.getProperties().put("Name", newName);   // Set new key
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
				sa.getId().toString().toString(),auxData,prodDb);
	}

	public void copyFamilyAuxData(SerializableFamily sf, String newName, String oldName) {
		
		String prodDb = requestHandler.getProductionDatabase();
		String isoDb = requestHandler.getIsolationDatabase();
//		ProcessDiagramView diagram = getActiveDiagram();
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();


		
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
	
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sf.getId(),auxData,isoDb);

		auxData.getProperties().put("Name", newName);   // Set new key

		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
				sf.getId().toString(),auxData,isoDb);
		
		auxData = new GeneralPurposeDataContainer();
		
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sf.getId().toString(),auxData,prodDb);

		auxData.getProperties().put("Name", newName);   // Set new key
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
				sf.getId().toString().toString(),auxData,prodDb);

	}


    
}
