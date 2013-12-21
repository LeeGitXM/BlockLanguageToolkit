package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import net.miginfocom.swing.MigLayout;

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
		List<BlockProperty> properties = handler.getBlockProperties(projectId,resourceId,block.getId(),block.getClassName());
		JPanel container = new JPanel(new MigLayout("flowy"));    // Container of a panel for each property
		if( properties!=null ) {
			for(BlockProperty property:properties) {
				PropertyPanel panel = new PropertyPanel(property);
				container.add(panel,"grow,push");
			}
		}
		
        

        //Create the scroll pane and add the table to it.
        JScrollPane scrollPane = new JScrollPane(container);
        //Add the scroll pane to this panel.
        add(scrollPane,BorderLayout.CENTER);
    }
	
	/**
	 * A property panel is an editor for a single property.
	 */
	@SuppressWarnings("serial")
	private class PropertyPanel extends JPanel {
		
		public PropertyPanel(BlockProperty prop) {
			setLayout(new MigLayout("fillx"));
			JLabel name = new JLabel(prop.getName());
			add(name,"wrap");
		}
		
	}
}


