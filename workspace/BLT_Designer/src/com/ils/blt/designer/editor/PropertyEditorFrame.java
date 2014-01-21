/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceAdapter;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.jidesoft.docking.DockableFrame;

/**
 * A Diagnostics workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DiagnosticsFrames. These are internal frames designed
 * to hold a model diagram. 
 * 
 */
@SuppressWarnings("serial")
public class PropertyEditorFrame extends DockableFrame implements ResourceWorkspaceFrame{
	private static final String TAG = "PropertyEditorFrame";
	public static final String DOCKING_KEY = "ProcessDiagramEditorFrame";
	public static final String TITLE = "Block Properties Editor";
	public static final String SHORT_TITLE = "Properties";
	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	private final JPanel contentPanel;
	
	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	
	/**
	 * Constructor 
	 */
	public PropertyEditorFrame(DesignerContext ctx,DiagramWorkspace workspace) {
		super(DOCKING_KEY, IconUtil.getRootIcon("delay_block_16.png"));  // Pinned icon
		this.context = ctx;
		this.workspace = workspace;
		workspace.addDesignableWorkspaceListener(new DiagramWorkspaceListener());
		contentPanel = new JPanel(new BorderLayout());
		init();	
	}


	@Override
	public String getKey() {
		return DOCKING_KEY;
	}

	/** 
	 * Initialize the UI components. The "master" version of the block's
	 * properties resides in the gateway.
	 */
	private void init() {
		setTitle(TITLE);
		setTabTitle(SHORT_TITLE);
		setSideTitle(SHORT_TITLE);
		
		setContentPane(contentPanel);
		contentPanel.setBorder(BorderFactory.createEtchedBorder());
	}
	

	@Override
	public boolean isInitiallyVisible() {
		return true;
	}
	
	private class DiagramWorkspaceListener extends DesignableWorkspaceAdapter {
		@Override
		public void itemSelectionChanged(List<JComponent> selections) {
			if( selections!=null && selections.size()==1 ) {
				JComponent selection = selections.get(0);
				log.infof("%s: DiagramWorkspaceListener: selected a %s",TAG,selection.getClass().getName());
				if( selection instanceof BlockComponent ) {
					BlockComponent bc = ( BlockComponent)selection;
					ProcessBlockView blk = (ProcessBlockView)bc.getBlock();
					PropertyEditor editor = new PropertyEditor(context,workspace.getActiveDiagram().getResourceId(),blk);
					contentPanel.removeAll();
					//Create a scroll pane
				    JScrollPane scrollPane = new JScrollPane(editor);
					contentPanel.add(scrollPane,BorderLayout.CENTER);
					return;
				}
			}
		}
	}
	
}
