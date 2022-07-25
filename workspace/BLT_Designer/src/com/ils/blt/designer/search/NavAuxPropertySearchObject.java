package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Return the text as a concatenation of property name and value from the block aux property list.
 *
 */
public class NavAuxPropertySearchObject implements SearchObject {
	private final String CLSS = "NavAuxListSearchObject";
	private final LoggerEx log;
	private static final Dimension IMAGE_SIZE = new Dimension(18,18);
	private final String name;
	private final String value;
	private final String parentPath;
	private final String nodeName;
	private final ProjectResourceId parentId;
	private final DesignerContext context;
	private final ResourceBundle rb;
	
	public NavAuxPropertySearchObject(DesignerContext ctx,String nam,String val,ProjectResourceId parent,String node) {
		this.context = ctx;
		this.name = nam;
		this.value = val;
		this.parentPath = parent.getResourcePath().getFolderPath();
		this.nodeName = node;
		this.parentId = parent;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
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
		return "AuxData: "+name;
	}

	@Override
	public String getOwnerName() {
		return parentPath+":"+nodeName;
	}


	@Override
	public String getText() {
		return name+":"+value;
	}

	@Override
	public void locate() {
		NavTreeLocator locator = new NavTreeLocator(context);
		if( parentId!=null) {
		locator.locate(parentId,nodeName);
		}
		else {
			locator.locate(nodeName);
		}
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning(rb.getString("Locator.AuxChangeWarning"),rb.getString("Locator.WarningTitle") ,false);
	}

}
