package com.ils.blt.designer.workspace.ui;

import java.util.Collection;

import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;


/**
 * Define the methods required for a block UI renderer in the designer.
 */
public interface BlockViewUI {

	public void install(BlockComponent block);
	public Collection<AnchorPoint> getAnchorPoints();   // actually BasicAnchorPoints
	public BlockComponent getBlockComponent();
}
