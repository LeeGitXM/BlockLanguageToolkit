package com.ils.blt.designer.workspace;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * An instance of this class should be run in the UI thread to cause a repaint of the current workspace
 * with the appropriate background color.
 * @author chuckc
 *
 */
public class WorkspaceBackgroundRepainter implements Runnable {
	private static final String CLSS = "WorkspaceBackgroundRepainter";
	private static DesignerContext context = null;
			
	public WorkspaceBackgroundRepainter() {
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * Note: this is called before the workspace is created.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	
	@Override
	public void run() {
		DiagramWorkspace workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		if( workspace!=null) {
			workspace.repaint(200);  // Paint in 200 ms
		}
	}
}
