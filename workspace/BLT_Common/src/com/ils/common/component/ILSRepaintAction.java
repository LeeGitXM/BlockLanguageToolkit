/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.common.component;

import javax.swing.SwingUtilities;

import prefuse.Visualization;
import prefuse.action.Action;
/**
 * Replace the Prefuse Repaint action to guarantee execution
 * on the Swing EDT thread.
 */
public class ILSRepaintAction extends Action implements Runnable {
    /**
     * Create a new RepaintAction.
     */
    public ILSRepaintAction() {
        super();
    }
    
    /**
     * Create a new RepaintAction.
     * @param vis the Visualization to repaint
     */
    public ILSRepaintAction(Visualization vis) {
        super(vis);
    }
    // f (frac) is unused.
	@Override
	public void run(double f) {
		SwingUtilities.invokeLater(this);
	}
	
	@Override
	public void run() {
		getVisualization().repaint();
		
	}
}
