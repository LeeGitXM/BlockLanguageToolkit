package com.ils.blt.designer.workspace;

import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Extend an AnchorDescriptor to include the idea of connection type
 * and port annotation.
 */
public class ProcessAnchorDescriptor extends AnchorDescriptor {
	private final ConnectionType connectionType;
	private final String annotation;

	
	public ProcessAnchorDescriptor(AnchorType type,ConnectionType ctype, Object id,String display,String note) {
		super(type,id,display);
		this.connectionType = ctype;
		this.annotation = note;
	}


	public ConnectionType getConnectionType() {return connectionType;}
	public String getAnnotation() { return annotation; }

}
