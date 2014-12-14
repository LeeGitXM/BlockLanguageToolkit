package com.ils.blt.client.component.recmap;

import java.awt.event.MouseEvent;

import javax.swing.JDialog;
import javax.swing.SwingUtilities;

import prefuse.controls.Control;
import prefuse.controls.ControlAdapter;
import prefuse.visual.VisualItem;


/**
 * A control that launches an edit dialog on selection of a "link" node.
 */
public class RecMapSelector extends ControlAdapter implements Control {
   
    /**
     * @see prefuse.controls.Control#itemClicked(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemClicked(VisualItem item, MouseEvent e) {
    	int nodeType = item.getInt(RecMapConstants.KIND);
    	if( nodeType==RecMapConstants.LINK_KIND) {
    		final JDialog editor = (JDialog)new LinkValueEditor(item);
        	editor.pack();
    		SwingUtilities.invokeLater(new Runnable() {
    			public void run() {
    				editor.setLocationByPlatform(true);
    				editor.setVisible(true);
    			}
    		}); 
    	}
    } 
} 
