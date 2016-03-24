package com.ils.blt.client.component.recmap;

import java.awt.event.MouseEvent;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JOptionPane;

import prefuse.controls.Control;
import prefuse.controls.ControlAdapter;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableNodeItem;


/**
 * A control that launches an edit dialog on selection of a "link" node.
 */
public class RecMapSelector extends ControlAdapter implements Control {
	private final RecommendationMap map;
	private final int clickCount;
	private int clicks;
	
	public RecMapSelector(RecommendationMap rm,int c) {
		this.map = rm;
		this.clickCount = c;
		clicks = 0;
	}
   
    /**
     * @see prefuse.controls.Control#itemClicked(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
	public void itemClicked(final VisualItem item, final MouseEvent e) {
		if( !e.isControlDown() ) return;
		if( item instanceof TableNodeItem ) {
			int nodeType = item.getInt(RecMapConstants.KIND);
			if( nodeType==RecMapConstants.INFO_KIND) {
				if( clicks==0 ) {
					// Set a timer to wait for the correct number of clicks
					Timer t = new Timer("clickTimer",false);
					t.schedule(new TimerTask() {
						@Override
						public void run() {
							if( clicks==clickCount) {
								//Window root = SwingUtilities.getWindowAncestor(map);
								String message = "Enter multiplier:";
								String ans = JOptionPane.showInputDialog(map, message, item.getString(RecMapConstants.MULTIPLIER));
								if( ans!=null) {
									item.setString(RecMapConstants.MULTIPLIER, ans);
									map.updateDiagnosis(item.getInt(RecMapConstants.INDEX),ans);
								}
							}
							clicks = 0;
						}
					}, 500);
				}
				clicks++;
			}
		}
	} 
} 
