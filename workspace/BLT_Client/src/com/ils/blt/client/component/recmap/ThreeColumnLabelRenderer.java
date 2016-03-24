package com.ils.blt.client.component.recmap;

import prefuse.render.LabelRenderer;
import prefuse.visual.VisualItem;


/**
 * This is a label renderer that selects the table column to be rendered
 * based on the column type of the node. This is very specific to the
 * three column layout.
 */
public class ThreeColumnLabelRenderer extends LabelRenderer {

    /**
     * Create a new LabelRenderer. The column "KIND"
     * determines which of "NAME" or "VALUE" to display.
     */
    public ThreeColumnLabelRenderer() {
    }
   
    
    /**
     * Returns the text to draw. Subclasses can override this class to
     * perform custom text selection.
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    protected String getText(VisualItem item) {
        String s = null;
        if ( item.canGetString(RecMapConstants.KIND) ) {
        	int kind = item.getInt(RecMapConstants.KIND);
        	// If this is the "link", then use the VALUE
        	if( kind==RecMapConstants.INFO_KIND) {
        		s = item.getString(RecMapConstants.VALUE); 
        	}
        	else {
        		s = item.getString(RecMapConstants.NAME); 
        	}
            return s;           
        }
        return s;
    }
} 
