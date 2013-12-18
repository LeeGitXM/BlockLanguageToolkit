package com.ils.blt.designer.workspace.ui;

import java.util.Collection;

import javax.swing.Icon;
import javax.swing.JPanel;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.BasicBlockUI;



public class StandardUIView extends BasicBlockUI implements BlockViewUI{
	private static final long serialVersionUID = -8677471243739333505L;

	public StandardUIView(ProcessBlockView view) {
		super(view);

	}

	@Override
	public Collection<AnchorDescriptor> getAnchors() {
		return ((ProcessBlockView)getBlock()).getAnchors();
	}

	@Override
	protected Icon getIcon() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String getTitle() {
		return "BLOCK";
	}

	@Override
	protected boolean isDetailsPanelRequired() {
		return true;
	}
	@Override
	protected void initDetailsPanel(JPanel details) {
		// TODO Auto-generated method stub
		super.initDetailsPanel(details);
	}

}
