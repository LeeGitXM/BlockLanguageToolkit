package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the diagram name for editing.
 * @author chuckc
 *
 */
public class FamilyNameSearchObject implements SearchObject {
	private final String TAG = "FamilyNameSearchObject";
	private final ILSLogger log;
	private static final Dimension IMAGE_SIZE = new Dimension(18,18);
	private final String applicationName;
	private final String familyName;
	private final ProjectResourceId parentId;
	private final DesignerContext context;
	private final ResourceBundle rb;
	
	public FamilyNameSearchObject(DesignerContext ctx,String app,String fam,ProjectResourceId parentUUID) {
		this.context = ctx;
		this.applicationName = app;
		this.familyName = fam;
		this.parentId = parentUUID;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.log = LogMaker.getLogger(this);
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage("Block/icons/navtree/family_folder_closed.png",IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		return icon;
	}

	@Override
	public String getName() {
		return familyName;
	}

	@Override
	public String getOwnerName() {
		return applicationName;
	}

	@Override
	public String getText() {
		return familyName;
	}

	@Override
	public void locate() {
		NavTreeLocator locator = new NavTreeLocator(context);
		locator.locate(parentId,familyName);
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning(rb.getString("Locator.FamilyChangeWarning"),rb.getString("Locator.WarningTitle") ,false);
	}

}
