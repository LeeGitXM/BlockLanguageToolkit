package com.ils.blt.designer.workspace.ui;

import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * Create a proper UI rendering class given the block style.
 */
public class UIFactory {
	private final static String TAG = "UIFactory";
	private final LoggerEx log;
	public UIFactory() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	public BlockViewUI getUI(BlockStyle style,ProcessBlockView block) {
      
		BlockViewUI ui = null;
		switch(style) {
			case ARROW:
				ui= new ArrowUIView(block);
				break;
			case CLAMP:
				ui= new ClampUIView(block);
				break;
			case DIAMOND:
				ui= new DiamondUIView(block);
				break;
			case LOGIC_AND:
				ui= new LogicUIView(block,LogicUIView.SUBSTYLE_AND);
				break;
			case LOGIC_NOT:
				ui= new LogicUIView(block,LogicUIView.SUBSTYLE_NOT);
				break;
			case LOGIC_OR:
				ui= new LogicUIView(block,LogicUIView.SUBSTYLE_OR);
				break;
			case ICON:
				ui= new IconUIView(block);
				break;
			case JUNCTION:
				ui= new JunctionUIView(block);
				break;
			case READOUT:
				ui= new ReadoutUIView(block);
				break;
			case ROUND:
				ui= new RoundUIView(block);
				break;
			case SQUARE:
				ui= new SquareUIView(block);   // Default
				break;
			case NOTE:
				ui= new NoteUIView(block);   // Default
				break;
			default:
				log.warnf("%s: getUI: Unrecognized style (%s)",TAG,style.toString());
				ui= new SquareUIView(block);
		}
		log.tracef("%s: getUI: Created style (%s)",TAG,style.toString());
		return ui;
	}
}
