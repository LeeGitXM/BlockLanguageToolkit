/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import prefuse.render.LabelRenderer;
import prefuse.visual.VisualItem;

/**
 * Render the block that holds recommendations.
 */
//public class DiagnosisRenderer extends TableLabelRenderer {
	
public class DiagnosisRenderer extends LabelRenderer {
    /**
     */
    public DiagnosisRenderer() {
    }
    
    /**
     * Returns the text to draw. Subclasses can override this class to
     * perform custom text selection.
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    protected String getText(VisualItem item) {
        String s = null;
        s = item.getString(RecMapConstants.NAME); 
        return s;
    }
} 
