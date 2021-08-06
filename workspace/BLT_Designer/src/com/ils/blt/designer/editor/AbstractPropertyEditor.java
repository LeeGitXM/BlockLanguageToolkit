/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.editor;

import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;

/**
 * This abstract class, adds a shutdown method to a SlidingPane (a JPanel) used as content for the property editor
 * frame in the designer.
 */
public abstract class AbstractPropertyEditor extends SlidingPane  {	
	private static final long serialVersionUID = 366366366;
	protected final ProjectResource resource;
	
	public AbstractPropertyEditor(ProjectResource res) {
		this.resource = res;
	}
	public AbstractPropertyEditor() {
		this.resource = null;
	}
	
	public ProjectResource getResource() { return this.resource; }
	/**
	 * Save the project resource. This stub method is provided for
	 * editors that are not associated with a project resource.
	 */
	public void saveResource() {
	}
	/**
	 * Stop any current processing.
	 */
	abstract public void shutdown();

}