package com.ils.common.component.recmap;

import java.awt.event.MouseEvent;
import java.util.Map;

import javax.swing.JPopupMenu;

import prefuse.controls.Control;
import prefuse.controls.ControlAdapter;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableNodeItem;


/**
 * A control that launches an edit dialog on selection of a "link" node.
 */
public class RecMapSelector extends ControlAdapter implements Control {
	private final static String CLSS = "RecMapSelector";
	private final RecommendationMap map;
	private final Map<Integer,TextDelegate> delegates;
	
	public RecMapSelector(RecommendationMap rm,Map<Integer,TextDelegate> textDelegates) {
		this.map = rm;
		this.delegates = textDelegates;
	}
   
	
	
    /**
     * On a double-click of a diagnosis block, display a dialog requesting the multiplier.
     * @see prefuse.controls.Control#itemClicked(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
	@Override
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
								int row = item.getInt(RecMapConstants.ROW);
								Properties properties = propertyMap.get(new Integer(row));
								String message = "Enter multiplier:";
								String value = "";
								if( properties!=null ) {
									if( properties.get(RecMapConstants.MULTIPLIER)!=null ) {
										value = properties.getProperty(RecMapConstants.MULTIPLIER);
									}
									else {
										log.warnf("%s.itemClicked: Missing field \"%s\" in properties",CLSS,RecMapConstants.MULTIPLIER);
									}
								}
								else {
									log.warnf("%s.itemClicked: No properties for node row %d",CLSS,row);
								}
								String ans = JOptionPane.showInputDialog(map, message, value);
								if( ans!=null) {
									properties.setProperty(RecMapConstants.MULTIPLIER,ans);
									map.updateDiagnosis(item.getInt(RecMapConstants.DSROW),ans);
								}
								else {
									log.warnf("%s.itemClicked: Missing field \"%s\" in dataset - failed to set",CLSS,RecMapConstants.MULTIPLIER);
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
	*/
	
	/**
	 * On a mouse-pressed display a pop-up menu
	 */
	@Override
	public void itemPressed(final VisualItem item, final MouseEvent e) {
		if( item instanceof TableNodeItem ) {
			int nodeType = item.getInt(RecMapConstants.KIND);
			JPopupMenu menu = new JPopupMenu();
			TextDelegate delegate = delegates.get(nodeType);
			delegate.addMenuItems(item,menu);
			menu.show(e.getComponent(),e.getX(), e.getY());
		}
	}
} 
