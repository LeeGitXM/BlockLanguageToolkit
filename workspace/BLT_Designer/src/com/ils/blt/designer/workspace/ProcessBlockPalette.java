/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JPanel;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.designable.tools.AbstractDesignTool;
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
public class ProcessBlockPalette extends DockableFrame implements ResourceWorkspaceFrame{
	private static final long serialVersionUID = 4627016359409031941L;
	private static final String TAG = "DiagnosticsPalette";
	public static final String DOCKING_KEY = "DiagnosticBlocks";
	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	

	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	
	/**
	 * Constructor 
	 */
	public ProcessBlockPalette(DesignerContext ctx,DiagramWorkspace workspace) {
		super(DOCKING_KEY, IconUtil.getRootIcon("delay_block_16.png"));  // Pinned icon
		setUndockedBounds(new Rectangle(200, 100, 550, 130));
		this.context = ctx;
		setAutohideHeight(100);
		setAutohideWidth(120);
		setDockedHeight(100);
		setDockedWidth(120);
		
		this.workspace = workspace;

		JPanel panel = new JPanel();
		panel.add(new JButton(new PaletteEntry("block")));
		panel.add(new JButton(new PaletteEntry("circle")));
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
	
	private class PaletteEntry extends AbstractAction {
		private String text;
		public PaletteEntry(String label) {
			super(label);
			text = label;
			
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			
			if( workspace.getSelectedContainer()!=null ) {
				ProcessBlockView blk = null;
				if( text.equals("circle")) {
					 blk = new CircleProcessBlockView();  
				}
				else {
					blk = new ProcessBlockView();  
				}
				 // Add other kinds of blocks ...
				workspace.setCurrentTool(new InsertBlockTool(blk));
			}
		}
	}
	
	private class InsertBlockTool extends AbstractDesignTool {
		
		private final ProcessBlockView block;
		public InsertBlockTool(ProcessBlockView blk) {
			block = blk;
		}
		
		@Override
		public Cursor getCursor(Point point, int inputEventMask) {
	
			return Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);
		}
		
		@Override
		public void onPress(Point p, int modifiers) {
			BlockDesignableContainer c = (BlockDesignableContainer)findDropContainer(p);
			BlockDiagramModel model = c.getModel();
			block.setLocation(p);
			model.addBlock(block);
			workspace.setCurrentTool(workspace.getSelectionTool());
		}
	}
}
