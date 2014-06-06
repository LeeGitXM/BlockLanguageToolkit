package com.ils.blt.designer.workspace;

import com.ils.block.common.PlacementHint;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Extend an AnchorDescriptor to include the idea of connection type,
 * port annotation and placement hints.
 */
public class ProcessAnchorDescriptor extends AnchorDescriptor {
	private final ConnectionType connectionType;
	private final String annotation;
	private final PlacementHint hint;

	
	public ProcessAnchorDescriptor(AnchorType type,ConnectionType ctype, Object id,String display,String note,PlacementHint placementHint) {
		super(type,id,display);
		this.connectionType = ctype;
		this.annotation = note;
		this.hint = placementHint;
	}


	public ConnectionType getConnectionType() {return connectionType;}
	public String getAnnotation() { return annotation; }
	public PlacementHint getHint() { return hint; }

}
