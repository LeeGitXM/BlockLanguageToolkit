package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the diagram name for editing.
 * @author chuckc
 *
 */
public class DiagramNameSearchObject implements SearchObject {
	private static final Dimension IMAGE_SIZE = new Dimension(18,18);
	private final ProcessDiagramView diagram;
	private final String familyName;
	private final DesignerContext context;
	private final ResourceBundle rb;
	
	public DiagramNameSearchObject(DesignerContext ctx,String fam,ProcessDiagramView dia) {
		this.context = ctx;
		this.diagram = dia;
		this.familyName = fam;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
	}
	@Override
	public Icon getIcon() {
		ImageIcon icon = null;
		Image img = ImageLoader.getInstance().loadImage("Block/icons/navtree/diagram.png",IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		return icon;
	}

	@Override
	public String getName() {
		return diagram.getDiagramName();
	}

	@Override
	public String getOwnerName() {
		return familyName;
	}

	@Override
	public String getText() {
		return diagram.getDiagramName();
	}

	@Override
	public void locate() {
		NavTreeLocator locator = new NavTreeLocator(context);
		locator.locate(diagram.getId());
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		ErrorUtil.showWarning(rb.getString("Locator.DiagramChangeWarning"),rb.getString("Locator.WarningTitle") ,false);
	}

}
