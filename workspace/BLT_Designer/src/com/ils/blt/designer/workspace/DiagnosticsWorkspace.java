/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.Dimension;
import java.beans.PropertyVetoException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.EditActionHandler;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspace;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.jidesoft.action.CommandBar;
import com.jidesoft.action.DockableBarManager;
import com.jidesoft.docking.DockContext;
import com.jidesoft.docking.DockableFrame;
import com.jidesoft.docking.DockingManager;

/**
 * A Diagnostics workspace is a container that occupies the DockManager workspace
 * area. It, in turn, holds DiagnosticsFrames. These are internal frames designed
 * to hold a model diagram. 
 * 
 * Implementing a ResourceWorkspace allows this the co-exist with other uses of the
 * central workspace in the Designer. This class is implemented as a Singleton for
 * easy access from disparate parts of the application.
 */
public class DiagnosticsWorkspace extends JDesktopPane implements ResourceWorkspace {
	private static final long serialVersionUID = 4627016159409031941L;
	private static final String TAG = "DiagnosticsWorkspace";
	private static final String KEY = "DiagnosticsWorkspace";
	private static DiagnosticsWorkspace instance = null;
	private static final int DEFAULT_WIDTH = 870;
	private static final int DEFAULT_HEIGHT = 640;

	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static DiagnosticsWorkspace getInstance() {
		if( instance==null) {
			synchronized(DiagnosticsWorkspace.class) {
				instance = new DiagnosticsWorkspace();
			}
		}
		return instance;
	}
	/**
	 * Constructor is private per the Singleton pattern.
	 */
	private DiagnosticsWorkspace() {
		setPreferredSize(new Dimension(DEFAULT_WIDTH,DEFAULT_HEIGHT));
	}
	/**
	 * If the designated frame already exists, then hide it.
	 * Otherwise do nothing.
	 * 
	 * @param frame
	 */
	public void close(DiagnosticsFrame frame) {
		if( frame==null ) return;  // Ignore
		
		String frameName = frame.getName();
		for(JInternalFrame frm:getAllFrames()) {
			if( frameName.equals(frm.getName()) ) {
				try {
					frm.setSelected(false);
				}
				catch(PropertyVetoException pve) {}
				frm.setVisible(false);
				break;
			}
		}
	}
	
	/**
	 * 
	 * @param frame
	 */
	public void open(DiagnosticsFrame frame) {
		if( frame==null ) return;  // Ignore
		
		boolean frameFound = false;
		String frameName = frame.getName();
		log.infof("%s: open %s",TAG,frameName);
		if( frameName==null) return;              // Problem
		for(JInternalFrame frm:getAllFrames()) {
			if( frameName.equals(frm.getName()) ) {
				frameFound = true;
				frm.setVisible(true);
				try {
					frm.setSelected(false);
				}
				catch(PropertyVetoException pve) {}
				break;
			}
		}
		// If the frame is not in the current collection, add it.
		if( !frameFound ) {
			frame.setVisible(true);
			add(frame);
			try {
				frame.setSelected(false);
			}
			catch(PropertyVetoException pve) {}
		};
	}
	
	@Override
	public EditActionHandler getEditActionHandler() {
		return null;
	}
	@Override
	public Collection<ResourceWorkspaceFrame> getFrames() {
		return new ArrayList<ResourceWorkspaceFrame>();
	}
	@Override
	public String getKey() {
		return KEY;
	}
	@Override
	public MenuBarMerge getMenu() {
		return null;
	}
	@Override
	public List<CommandBar> getToolbars() {
		return new ArrayList<CommandBar>();
	}
	@Override
	public JComponent getWorkspace() {
		return this;
	}
	/**
	 * Show the "tool" frames that we'd like to see while the diagnostics "view" is active.
	 * Note that the project browser and tags browsers are set by default.
	 */
	@Override
	public void resetFrames(DockingManager dockingManager, DockableBarManager barManager) {
		log.infof("%s: resetFrames ...",TAG);
		DockableFrame props = dockingManager.getFrame("PropertyTablePanel");
		props.setInitIndex(2);
		props.setInitMode(DockContext.STATE_FRAMEDOCKED);
		props.setInitSide(DockContext.DOCK_SIDE_WEST);
		DockableFrame console = dockingManager.getFrame("Console");
		console.setInitIndex(3);
		console.setInitMode(DockContext.STATE_FRAMEDOCKED);
		console.setInitSide(DockContext.DOCK_SIDE_WEST);
	}
}
