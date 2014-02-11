package com.ils.blt.designer.workspace.ui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JPanel;

import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.BasicBlockUI;


/**
 * WARNING: This is class is incompatible with the Block Language Toolkit.
 * Its ports do not know about connection style.
 */
public class StandardUIView extends BasicBlockUI implements BlockViewUI{
	private static final long serialVersionUID = -8677471243733333505L;
	private List<AnchorDescriptor> anchorDescriptors = null;  // Entries are BasicAnchorPoint

	public StandardUIView(ProcessBlockView view) {
		super(view);
		anchorDescriptors = new ArrayList<AnchorDescriptor>();
		for(ProcessAnchorDescriptor desc:view.getAnchors()) {
			// Create a local anchor array from the block
			anchorDescriptors.add(desc);
		}
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

	/**
	 * We have to populate the anchor descriptors in the super constructor
	 */
	@Override
	public Collection<AnchorDescriptor> getAnchors() {
		if( anchorDescriptors==null ) {
			anchorDescriptors = new ArrayList<AnchorDescriptor>();
			for(ProcessAnchorDescriptor desc:((ProcessBlockView)super.getBlock()).getAnchors()) {
				// Create a local anchor array from the block
				anchorDescriptors.add(desc);
			}
		}
		return anchorDescriptors;
	}

}
