package com.ils.blt.designer.workspace;

import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.connection.ConnectionType;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Extend an AnchorDescriptor to include the idea of connection type,
 * port annotation and placement hints.
 */
public class ProcessAnchorDescriptor extends AnchorDescriptor {
	private ConnectionType connectionType;
	private final String annotation;
	private PlacementHint hint;
	private final boolean multiple;
	private QualifiedValue lastValue = null;

	
	public ProcessAnchorDescriptor(AnchorType type,ConnectionType ctype, Object id,String display,String note,PlacementHint placementHint,boolean supportsMultiple) {
		super(type,id,display);
		this.connectionType = ctype;
		this.annotation = note;
		this.hint = placementHint;
		this.multiple = supportsMultiple;
	}


	public ConnectionType getConnectionType() {return connectionType;}
	public String getAnnotation() { return annotation; }
	public PlacementHint getHint() { return hint; }
	public QualifiedValue getLastValue() { return lastValue; }
	public boolean isMultiple() { return multiple; }
	/**
	 * Allow changes to the connection type for the rare instances where the
	 * user wishes to customize a block before it has ever been saved.
	 * @param ct
	 */
	public void setConnectionType(ConnectionType ct) { this.connectionType=ct; }
	/**
	 * We use this internally in the AbstractUIView to mark out placement definitively.
	 */
	public void setHint(PlacementHint h) { this.hint = h; }
	/**
	 * Specify the most recent value emitted at this port.
	 * @param qv
	 */
	public void setLastValue(QualifiedValue qv) { this.lastValue = qv; }
}
