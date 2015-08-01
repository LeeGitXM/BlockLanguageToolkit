/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.connection.ConnectionType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * Implement a plain-old-java-object representing an anchor point
 * that is serializable via a JSON serializer. This class is used 
 * whenever a block is serialized.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableAnchor {
	private final static String TAG = "SerializableAnchor";
	private AnchorDirection direction;   // 0=>Origin, 1=>Terminus
	private Object id = null;            // The port name
	private String display = null;
	private UUID parentId = null;
	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private String annotation = "";
	private ConnectionType connectionType = ConnectionType.ANY;
	private PlacementHint hint = PlacementHint.UNSPECIFIED;
	private boolean hidden = false;
	private boolean multiple = true;

	public SerializableAnchor() {
	}
	
	public String getAnnotation() {return annotation;}
	public ConnectionType getConnectionType() {return connectionType;}
	public Object getId() { return id; }
	public AnchorDirection getDirection()   { return direction; }
	public String getDisplay(){ return display; }
	public PlacementHint getHint() {return hint;}
	public UUID getParentId() { return parentId; }
	public boolean isHidden() {return hidden;}
	public boolean isMultiple() {return multiple;}

	public void setId(Object identifier) { id=identifier; }
	public void setAnnotation(String note) { this.annotation = note; }
	public void setConnectionType(ConnectionType connectionType) {this.connectionType = connectionType;}
	public void setDirection(AnchorDirection t)   { direction=t; }
	public void setDisplay(String text){display=text; }
	public void setHidden(boolean hidden) {this.hidden = hidden;}
	public void setHint(PlacementHint hint) {this.hint = hint;}
	public void setMultiple(boolean multiple) {this.multiple = multiple;}
	public void setParentId(UUID id) { parentId = id; };

	// So that class may be used as a map key
	// Same name and parent is sufficient to prove equality
	@Override
	public SerializableAnchor clone() {
		SerializableAnchor newAnchor = new SerializableAnchor();
		newAnchor.setId(UUID.randomUUID());
		newAnchor.setAnnotation(annotation);
		newAnchor.setConnectionType(connectionType);
		newAnchor.setDirection(direction);
		newAnchor.setDisplay(display);
		newAnchor.setHidden(hidden );
		newAnchor.setHint(hint);
		newAnchor.setMultiple(multiple);
		newAnchor.setParentId(parentId);
		return newAnchor;
	}
	// So that class may be used as a map key
	// Same name and parent is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof SerializableAnchor) {
			SerializableAnchor that = (SerializableAnchor)arg;
			if( this.id.equals(that.id) &&
				this.parentId.equals(that.getParentId())   ) {
				result = true;
			}
		}
		log.trace(toString()+" equals "+arg.toString()+" "+result);
		return result;
	}
	@Override
	public int hashCode() {
		int code = 42;
		if( parentId!=null) code += parentId.hashCode();
		if( display!=null ) code += display.hashCode();
		return code;
	}
	
	@Override
	public String toString() {
		return String.format("%s: %s (%s)",TAG,id.toString(),(direction==AnchorDirection.INCOMING?"Incoming":"Outgoing"));
	}
}
