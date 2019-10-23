package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the block name for editing.
 * @author chuckc
 *
 */
public class BlockNameSearchObject implements SearchObject {
	private final DesignerContext context;
	private static final Dimension IMAGE_SIZE = new Dimension(18,18);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final ResourceBundle rb;
	
	public BlockNameSearchObject(DesignerContext ctx,ProcessDiagramView parent, ProcessBlockView blk) {
		this.context = ctx;
		this.diagram = parent;
		this.block = blk;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
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
		return block.getName();
	}

	@Override
	public String getOwnerName() {
		return diagram.getName();
	}

	@Override
	public String getText() {
		return block.getClassName() + " " + block.getName();
	}

	// We navigate to the diagram.
	@Override
	public void locate() {
		NavTreeLocator locator = new NavTreeLocator(context);
		locator.locate(diagram.getId());
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning(rb.getString("Locator.BlockChangeWarning"),rb.getString("Locator.WarningTitle") ,false);
		
	}

}
