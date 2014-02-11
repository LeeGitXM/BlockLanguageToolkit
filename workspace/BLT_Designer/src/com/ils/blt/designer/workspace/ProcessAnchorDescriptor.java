package com.ils.blt.designer.workspace;

import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Extend an AnchorDescriptor to include the idea of connection type.
 */
public class ProcessAnchorDescriptor extends AnchorDescriptor {
	private final ConnectionType connectionType; 

	
	public ProcessAnchorDescriptor(AnchorType type,ConnectionType ctype, Object id,String display) {
		super(type,id,display);
		this.connectionType = ctype;
	}


	public ConnectionType getConnectionType() {return connectionType;}


}
