package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the block name for editing.
 * @author chuckc
 *
 */
public class BlockNameSearchObject implements SearchObject {
	private final DesignerContext context;
	private static final Dimension IMAGE_SIZE = new Dimension(32,32);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	
	public BlockNameSearchObject(DesignerContext ctx,ProcessDiagramView parent, ProcessBlockView blk) {
		this.context = ctx;
		this.diagram = parent;
		this.block = blk;
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage(block.getIconPath(),IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		return icon;
	}

	@Override
	public String getName() {
		return block.getName();
	}

	/**
	 * This should be a path to the object.
	 */
	@Override
	public String getOwnerName() {
		return diagram.getName();
	}

	@Override
	public String getText() {
		return block.getName();
	}

	@Override
	public void locate() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		// TODO Auto-generated method stub
		
	}

}
