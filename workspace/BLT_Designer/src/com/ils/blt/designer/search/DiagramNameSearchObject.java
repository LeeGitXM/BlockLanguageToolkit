package com.ils.blt.designer.search;

import java.awt.Dimension;
import java.awt.Image;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the diagram name for editing.
 * @author chuckc
 *
 */
public class DiagramNameSearchObject implements SearchObject {
	private static final Dimension IMAGE_SIZE = new Dimension(32,32);
	private final ProcessDiagramView diagram;
	private final DesignerContext context;
	
	public DiagramNameSearchObject(DesignerContext ctx,ProcessDiagramView dia) {
		this.context = ctx;
		this.diagram = dia;
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
		return BLTProperties.MODULE_NAME;
	}

	@Override
	public String getText() {
		return diagram.getDiagramName();
	}

	@Override
	public void locate() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		throw new IllegalArgumentException("The name of a diagram may be changed only in the Designer navigation tree");
	}

}
