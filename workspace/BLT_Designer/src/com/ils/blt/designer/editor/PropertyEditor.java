package com.ils.blt.designer.editor;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import com.ils.block.common.BlockProperty;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.PropertiesRequestHandler;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Display a dialog to edit cell attributes.
 *    BlockAttributeDialog bad = new BlockAttributeDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 *    
 */

public class PropertyEditor extends JPanel {
	private final static String TAG = "PropertyEditor";
	private static final long serialVersionUID = 8971626415423709616L;
	private ProcessBlockView block;
	private final DesignerContext context;
	private final LoggerEx log;
	private final long projectId;
	private final long resourceId;
	
	/**
	 * @param view the designer version of the block to edit. We 
	 */
	public PropertyEditor(DesignerContext ctx,long res,ProcessBlockView view) {
		this.context = ctx;
		this.projectId = ctx.getProject().getId();
		this.resourceId = res;
		this.block = view;
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        init();    
	}

	
	/** 
	 * Initialize the UI components. The "master" version of the block's
	 * properties resides in the gateway.
	 */
	private void init() {
		// Get the block attributes from the gateway. If this is a newly
		// created block, the gateway will create it.
		PropertiesRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
		handler.getBlockProperties(projectId,resourceId,block.getId());
		JPanel panel = new JPanel();    // Container of a panel for each property
		
        

        //Create the scroll pane and add the table to it.
        JScrollPane scrollPane = new JScrollPane(panel);
        //Add the scroll pane to this panel.
        add(scrollPane,BorderLayout.CENTER);
    }
	
	/**
	 * A property panel is an editor for a single property.
	 */
	private class PropertyPanel extends JPanel {
		private final static String TAG = "AttributeModel";
		private static final long serialVersionUID = 3853127246234590508L;
		private static final int COLUMN_COUNT = 3;
	               // The keys don't change
		
		public PropertyPanel(BlockProperty prop) {
			
		}
		
	}
}


