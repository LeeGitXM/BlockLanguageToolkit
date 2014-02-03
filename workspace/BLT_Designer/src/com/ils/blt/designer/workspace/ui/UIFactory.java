package com.ils.blt.designer.workspace.ui;

import com.ils.block.common.BlockStyle;
import com.ils.blt.designer.workspace.ProcessBlockView;


/**
 * Create a proper UI rendering class given the block style.
 */
public class UIFactory {
	public UIFactory() {
		
	}
	
	public BlockViewUI getUI(BlockStyle style,ProcessBlockView block) {
		
		BlockViewUI ui = null;
		switch(style) {
			case DATABASE:
				ui= new DatabaseUIView(block);
				break;
			case DIAMOND:
				ui= new DiamondUIView(block);
				break;
			case ENTRY:
				ui= new EntryUIView(block);
				break;
			case JUNCTION:
				ui= new JunctionUIView(block);
				break;
			case ROUND:
				ui= new RoundUIView(block);
				break;
			case SQUARE:
				ui= new SquareUIView(block);
				break;
			case BASIC:
			default:
				ui= new StandardUIView(block);
		}
		return ui;
	}
}
