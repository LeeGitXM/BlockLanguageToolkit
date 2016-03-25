/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import java.awt.event.MouseEvent;
import java.util.HashMap;
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
	private final Map<Integer,TextDelegate> delegates;
	private final LoggerEx log;

	public RecMapTooltipControl() {
		this.delegates = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	public void setDelegate(int kind,TextDelegate delegate) {
		delegates.put(new Integer(kind),delegate);
	}
	
	@Override
    public void itemEntered(VisualItem item,MouseEvent event) {
		log.infof("%s.itemEntered ....",CLSS);
		if( item instanceof TableNodeItem ) {
			int kind = item.getInt(RecMapConstants.KIND);
			TextDelegate delegate = delegates.get(new Integer(kind));
			if( delegate!=null) {
				Display display = (Display)event.getSource();
				display.setToolTipText(delegate.getTooltipText(item));
			}
		}
    }
	
	@Override
    public void itemExited(VisualItem item,MouseEvent event) {
		Display display = (Display)event.getSource();
		display.setToolTipText(null);
    }
} 
