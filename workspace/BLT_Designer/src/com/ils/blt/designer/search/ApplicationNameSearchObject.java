package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the diagram name for editing.
 * @author chuckc
 *
 */
public class ApplicationNameSearchObject implements SearchObject {
	private final String TAG = "ApplicationNameSearchObject";
	private final LoggerEx log;
	private static final Dimension IMAGE_SIZE = new Dimension(20,20);
	
	private final String applicationName;
	private final String rootName;
	private final DesignerContext context;
	
	public ApplicationNameSearchObject(DesignerContext ctx,String root,String app) {
		this.context = ctx;
		this.applicationName = app;
		this.rootName = root;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage("Block/icons/navtree/application_folder.png",IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		return icon;
	}

	@Override
	public String getName() {
		return applicationName;
	}

	@Override
	public String getOwnerName() {
		return rootName;
	}

	@Override
	public String getText() {
		return applicationName;
	}

	@Override
	public void locate() {
		log.infof("%s.locate:  Need to navigate to: %s",TAG,applicationName);
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		throw new IllegalArgumentException("The name of a diagram may be changed only in the Designer navigation tree");
	}

}
