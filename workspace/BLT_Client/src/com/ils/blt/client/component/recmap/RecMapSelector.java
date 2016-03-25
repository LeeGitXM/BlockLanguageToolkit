package com.ils.blt.client.component.recmap;

import java.awt.event.MouseEvent;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JOptionPane;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.controls.Control;
import prefuse.controls.ControlAdapter;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableNodeItem;


/**
 * A control that launches an edit dialog on selection of a "link" node.
 */
public class RecMapSelector extends ControlAdapter implements Control {
	private final static String CLSS = "RecMapSelector";
	private final LoggerEx log;
	private final RecommendationMap map;
	private final int clickCount;
	private int clicks;
	
	public RecMapSelector(RecommendationMap rm,int c) {
		this.map = rm;
		this.clickCount = c;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		clicks = 0;
	}
   
    /**
     * On a double-click of a diagnosis block, display a dialog requesting the multiplier.
     * @see prefuse.controls.Control#itemClicked(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
	public void itemClicked(final VisualItem item, final MouseEvent e) {
		//if( !e.isControlDown() ) return;
		if( item instanceof TableNodeItem ) {
			int nodeType = item.getInt(RecMapConstants.KIND);
			if( nodeType==RecMapConstants.SOURCE_KIND) {
				if( clicks==0 ) {
					// Set a timer to wait for the correct number of clicks
					Timer t = new Timer("clickTimer",false);
					t.schedule(new TimerTask() {
						@Override
						public void run() {
							if( clicks==clickCount) {
								//Window root = SwingUtilities.getWindowAncestor(map);
								String message = "Enter multiplier:";
								String value = "";
								if( item.canGetDouble(RecMapConstants.MULTIPLIER) ) {
									value = String.valueOf(item.getDouble(RecMapConstants.MULTIPLIER));
								}
								else {
									log.warnf("%s.itemClicked: Missing field \"%s\" in dataset",CLSS,RecMapConstants.MULTIPLIER);
								}
								String ans = JOptionPane.showInputDialog(map, message, value);
								if( ans!=null) {
									if( item.canSetDouble(RecMapConstants.MULTIPLIER)) {
										try {
											double dbl = Double.parseDouble(ans);
											item.setDouble(RecMapConstants.MULTIPLIER,dbl);
											map.updateDiagnosis(item.getInt(RecMapConstants.INDEX),ans);
										}
										catch(NumberFormatException nfe) {
											log.warnf("%s.itemClicked: Could not parse \"%s\" as a double (%s)",CLSS,ans,nfe.getLocalizedMessage());
										}
									}
									else {
										log.warnf("%s.itemClicked: Missing field \"%s\" in dataset - failed to set",CLSS,RecMapConstants.MULTIPLIER);
									}
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
