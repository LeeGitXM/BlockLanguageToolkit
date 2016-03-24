/**
 *   (c) 2014-2016 ILS Automation. All rights reserved. 
 */
package com.ils.blt.client.component.recmap;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.render.LabelRenderer;
import prefuse.visual.VisualItem;


/**
 * A table-label is a multi-line label which displays text in separate
 * rows. The top-most row, the header, is shaded.
 * 
 * The value in the top row is the name of the block. Subsequent rows 
 * are populated from a list of name-value pairs, the attributes.
 */
public class TableLabelRenderer extends LabelRenderer {
	private static final String TAG = "TableLabelRenderer";
	private final LoggerEx log;

    /**
     */
    public TableLabelRenderer() {
    	this.log = LogUtil.getLogger(getClass().getPackage().getName());
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
        	s = item.getString(RecMapConstants.NAME); 
            return s;           
        }
        return s;
    }

} 
