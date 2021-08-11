/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * An EncapsulatedDiagramNode differs from a DiagramNode only in that it 
 * represents the sub-workspace of an encapsulation block. It is generally
 * not addable, nor deletable except through the process of adding or 
 * deleting its parent encapsulation block. It derives its name from this block.
 */
public class EncapsulatedDiagramTreeNode extends DiagramTreeNode  {
	private static final String TAG = "EncapsulatedDiagramNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final ProcessBlockView block;

	/**
	 * Constructor. An EncapsulatedDiagramNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param ws the tabbed workspace holding the diagrams
	 * @param view the process block view that is the parent block of this diagram
	 */
	public EncapsulatedDiagramTreeNode(DesignerContext context,ProjectResource resource,DiagramWorkspace ws,ProcessBlockView view) {
		super(context,resource,ws);
		this.block = view;
	}
}
