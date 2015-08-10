package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the diagram name for editing.
 * @author chuckc
 *
 */
public class FamilyNameSearchObject implements SearchObject {
	private final String TAG = "FamilyNameSearchObject";
	private final LoggerEx log;
	private static final Dimension IMAGE_SIZE = new Dimension(20,20);
	private final String applicationName;
	private final String familyName;
	private final DesignerContext context;
	
	public FamilyNameSearchObject(DesignerContext ctx,String app,String fam) {
		this.context = ctx;
		this.applicationName = app;
		this.familyName = fam;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage("Block/icons/navtree/family_folder.png",IMAGE_SIZE);
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
		log.infof("%s.locate:  Need to navigate to: %s",TAG,familyName);
		NavTreeLocator locator = new NavTreeLocator(context);
		locator.locate(familyName);
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning("The name of a family may be changed only in the Designer navigation tree", "Replace Failed Warning",false);
		throw new IllegalArgumentException("The name of a family may be changed only in the Designer navigation tree");
	}

}
