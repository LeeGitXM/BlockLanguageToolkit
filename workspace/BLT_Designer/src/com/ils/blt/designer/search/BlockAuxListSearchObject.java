package com.ils.blt.designer.search;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * We treat the list of maps as a single colon,comma-delimited string
 * @author chuckc
 *
 */
public class BlockAuxListSearchObject implements SearchObject {
	private final String CLSS = "BlockAuxListSearchObject";
	private final LoggerEx log;
	private static final Dimension IMAGE_SIZE = new Dimension(18,18);
	private final String key;
	private final List<String> list;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final DesignerContext context;
	private final ResourceBundle rb;
	
	public BlockAuxListSearchObject(DesignerContext ctx,String k,List<String> data,ProcessDiagramView parent, ProcessBlockView blk) {
		this.context = ctx;
		this.key = k;
		this.list = data;
		this.diagram = parent;
		this.block = blk;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage("Block/icons/palette/blank_analysis.png",IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		return icon;
	}

	@Override
	public String getName() {
		return "AuxData: "+key;
	}

	@Override
	public String getOwnerName() {
		return diagram.getName()+":"+block.getName();
	}

	@Override
	public String getText() {
		StringBuilder builder = new StringBuilder();
		for(String text:list) {
			if(builder.length()>0) builder.append(",");
			builder.append(text);
		}
		return builder.toString();
	}
	// Navigate to the diagram
	@Override
	public void locate() {
		NavTreeLocator locator = new NavTreeLocator(context);
		locator.locate(diagram.getResourceId());
		DiagramWorkspace workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();

	    BlockDesignableContainer container = workspace.getSelectedContainer();
	    Component[] blocks = container.getComponents();
		ArrayList<JComponent> blockList = new ArrayList<>();
		for( Component blocky:blocks) {
			
			if (blocky instanceof BlockComponent) {
				if (((BlockComponent) blocky).getBlock().getId().equals(block.getId())) {
					((BlockComponent) blocky).setSelected(true);
					blockList.add((BlockComponent)blocky);
					workspace.setSelectedItems(blockList);
				}
			}
		}
		workspace.setSelectedItems(blockList);  // should be a BlockComponent

		block.notify();
		block.getName(); // unique name for the block
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning(rb.getString("Locator.AuxChangeWarning"),rb.getString("Locator.WarningTitle") ,false);
	}

}
