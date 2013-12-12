/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceAdapter;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceListener;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.inductiveautomation.ignition.designer.scripteditor.component.CodeEditorFactory;
import com.jidesoft.docking.DockableFrame;
import com.jidesoft.editor.CodeEditor;

/**
 * A Diagnostics workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DiagnosticsFrames. These are internal frames designed
 * to hold a model diagram. 
 * 
 */
public class PropertyEditorFrame extends DockableFrame implements ResourceWorkspaceFrame{
	private static final String TAG = "PropertyEditorFrame";
	public static final String DOCKING_KEY = "ProcessDiagramEditorFrame";
	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	private JLabel label;
	

	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	
	/**
	 * Constructor 
	 */
	public PropertyEditorFrame(DesignerContext ctx,DiagramWorkspace workspace) {
		super(DOCKING_KEY, IconUtil.getRootIcon("delay_block_16.png"));  // Pinned icon
		this.label = new JLabel("Hi Carl");
		this.context = ctx;

		this.workspace = workspace;
		workspace.addDesignableWorkspaceListener(new DiagramWorkspaceListener());

		JPanel panel = new JPanel(new MigLayout("fill"));
		panel.add(label,"wrap");
		CodeEditor editor = CodeEditorFactory.newPythonEditor();
		panel.add(editor,"grow,push");
		
		setContentPane(panel);
	}


	@Override
	public String getKey() {
		return DOCKING_KEY;
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
				if( selection instanceof BlockComponent ) {
					BlockComponent bc = ( BlockComponent)selection;
					ProcessBlockView blk = (ProcessBlockView)bc.getBlock();
					label.setText(bc.getLocation().toString());
					return;
				}
			}
			label.setText("");
		}
	}
	
}
