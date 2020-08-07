/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.common.component.recmap;

import java.awt.event.MouseEvent;
import java.util.Map;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.Display;
import prefuse.controls.ControlAdapter;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableNodeItem;

/**
 * Render an edge with different thicknesses depending
 * on whether or not the edge is active.
 */
public class RecMapTooltipControl extends ControlAdapter {
	private final static String CLSS = "RecMapTooltipControl";
	private final static boolean DEBUG = true;
	private final Map<Integer,TextDelegate> delegates;
	private final RecMapDataModel model;
	private final LoggerEx log;

	public RecMapTooltipControl(RecMapDataModel mdl,Map<Integer,TextDelegate> textDelegates) {
		this.delegates = textDelegates;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.model = mdl;
	}
	
	/**
	 * Note: We've verified that the display has no custom tooltip class
	 */
	@Override
    public void itemEntered(VisualItem item,MouseEvent event) {
		if( item instanceof TableNodeItem ) {
			int kind = item.getInt(RecMapConstants.KIND);
			int row = item.getInt(RecMapConstants.ROW);
			if( DEBUG) {
				if( item.canGetString(RecMapConstants.NAME) ) {
					log.infof("%s.itemEntered %d = %s",CLSS,row,item.getString(RecMapConstants.NAME));
				}
				else {
					log.infof("%s.itemEntered %d = (no name)",CLSS,row);
				}
			}
			TextDelegate delegate = delegates.get(new Integer(kind));
			if( delegate!=null) {
				Display display = (Display)event.getSource();
				String tooltip = delegate.getTooltipText(item);
				display.setToolTipText(tooltip);
			}
		}
    }
	
	@Override
    public void itemExited(VisualItem item,MouseEvent event) {
		Display display = (Display)event.getSource();
		display.setToolTipText(null);
    }
} 
