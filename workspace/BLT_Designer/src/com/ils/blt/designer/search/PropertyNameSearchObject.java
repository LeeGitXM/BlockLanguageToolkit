package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * View the name of the property
 * @author chuckc
 *
 */
public class PropertyNameSearchObject implements SearchObject {
	private static final Dimension IMAGE_SIZE = new Dimension(18,18);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final DesignerContext context;
	private final BlockProperty property;
	private final ResourceBundle rb;
	
	public PropertyNameSearchObject(DesignerContext ctx,ProcessDiagramView grandparent,ProcessBlockView parent,BlockProperty prop) {;
		this.context = ctx;
		this.diagram = grandparent;
		this.block = parent;
		this.property = prop;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage("Block/icons/palette/blank.png",IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		return icon;
	}

	@Override
	public String getName() {
		return property.getName();
	}

	@Override
	public String getOwnerName() {
		return diagram.getDiagramName()+":"+block.getName();
	}

	@Override
	public String getText() {
		return property.getName();
	}

	@Override
	public void locate() {
		NavTreeLocator locator = new NavTreeLocator(context);
		locator.locate(diagram.getResourceId());
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning(rb.getString("Locator.PropertyChangeWarning"),rb.getString("Locator.WarningTitle") ,false);
		
	}

}
