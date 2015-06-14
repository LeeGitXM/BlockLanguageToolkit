package com.ils.blt.designer.workspace;

import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * An instance of this class should be run in the UI thread to cause a repaint of the current workspace.
 * @author chuckc
 *
 */
public class WorkspaceRepainter implements Runnable {
	private static DesignerContext context = null;
	private static DiagramWorkspace workspace = null;
			
	public WorkspaceRepainter() {
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * Note: this is called before the workspace is created.
	 * @param context
	 */
	public static void setup(DesignerContext ctx,DiagramWorkspace wksp) {
		context = ctx;
		workspace = wksp;
	}
	
	@Override
	public void run() {
		if( workspace!=null ) {
			workspace.repaint(200);  // Paint in 200 ms
		}
	}
}
