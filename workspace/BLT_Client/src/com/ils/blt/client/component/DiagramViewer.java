/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.Dataset;

/**
 *  Use Prefuse to display a diagnostic toolkit diagram. The core data structure
 *  is two datasets:
 *   blocks: a row for each block and with columns: UUID,class,x,y,
 *   connections: a row for each connection and with columns: UUID from, port from, UUID to, port to, connectionType
 */
public class DiagramViewer extends PrefuseViewerComponent {
	private static final long serialVersionUID = 5508313516136446100L;
	private static String PREFIX = BLTProperties.CUSTOM_PREFIX;       // For bundle identification
	private Dataset blocks = null;
	private Dataset connections = null;
	
	// These are the properties that are editable
	public static final String BLOCKS_PROPERTY          = "blocks";
	public static final String CONNECTIONS_PROPERTY     = "connections";

	public DiagramViewer() {
		
		setName(BundleUtil.get().getString(PREFIX+".DiagramViewer.Name"));
		this.setOpaque(true);
		this.setBorder(border);
	}

	// We need getters/setters for the bean info
	public Dataset getBlocks() {return blocks;}
	public void setBlocks(Dataset blocks) {this.blocks = blocks;}
	public Dataset getConnections() {return connections;}
	public void setConnections(Dataset connections) {this.connections = connections;}

}
