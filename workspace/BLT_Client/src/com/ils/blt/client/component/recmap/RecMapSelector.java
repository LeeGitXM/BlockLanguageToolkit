package com.ils.blt.client.component.recmap;

import java.awt.event.MouseEvent;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JDialog;
import javax.swing.SwingUtilities;

import prefuse.controls.Control;
import prefuse.controls.ControlAdapter;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableNodeItem;

import com.inductiveautomation.ignition.common.Dataset;


/**
 * A control that launches an edit dialog on selection of a "link" node.
 */
public class RecMapSelector extends ControlAdapter implements Control {
	private final int OFFSET = 20;
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
		if( e.isAltDown() || e.isAltGraphDown() || e.isControlDown() || e.isMetaDown() ) return;
		if( item instanceof TableNodeItem ) {
			int nodeType = item.getInt(RecMapConstants.KIND);
			if( nodeType==RecMapConstants.LINK_KIND) {
				if( clicks==0 ) {
					// Set a timer to wait for the correct number of clicks
					Timer t = new Timer("clickTimer",false);
					t.schedule(new TimerTask() {
						@Override
						public void run() {
							if( clicks==clickCount) {
								final LinkValueEditor editor = new LinkValueEditor(item);
								editor.pack();
								editor.setLocation(e.getX()+OFFSET, e.getY()+OFFSET);
								SwingUtilities.invokeLater(new Runnable() {
									public void run() {
										editor.setVisible(true);
										String val = editor.getEditedValue();
										map.updateRecommendations(item.getInt(RecMapConstants.INDEX),val);
									}
								}); 
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
