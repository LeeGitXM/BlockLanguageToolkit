package com.ils.blt.designer.search;

import javax.swing.Icon;

import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Simply return the diagram name for editing.
 * @author chuckc
 *
 */
public class DiagramNameSearchObject implements SearchObject {
	private final ProcessDiagramView diagram;
	private final DesignerContext context;
	
	public DiagramNameSearchObject(DesignerContext ctx,ProcessDiagramView dia) {
		this.context = ctx;
		this.diagram = dia;
	}
	@Override
	public Icon getIcon() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getName() {
		return diagram.getDiagramName();
	}

	@Override
	public String getOwnerName() {
		return "Owner";
	}

	@Override
	public String getText() {
		// TODO Auto-generated method stub
		return diagram.getDiagramName();
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
