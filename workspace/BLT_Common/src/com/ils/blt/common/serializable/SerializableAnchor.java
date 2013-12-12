package com.ils.blt.common.serializable;


/**
 * Implement a plain-old-java-object representing an anchor point
 * that is serializable via the Ignition XML serializer.
 */
public class SerializableAnchor {
	private int type;   // 0=>Origin, 1=>Terminus
	private Object id = null;
	private String display = null;
	
	public SerializableAnchor  () {
	}
	
	public Object getId() { return id; }
	public int getType()   { return type; }
	public String getDisplay(){ return display; }
	
	public void setId(Object identifier) { id=identifier; }
	public void setType(int t)   { type=t; }
	public void setDisplay(String text){display=text; }

}
