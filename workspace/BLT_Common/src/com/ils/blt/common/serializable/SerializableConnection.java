package com.ils.blt.common.serializable;

import java.util.UUID;

/**
 * Implement a plain-old-java-object representing a connection
 * that is serializable via the Ignition XML serializer.
 */
public class SerializableConnection {
	private UUID beginBlock = null;
	private UUID endBlock = null;
	private String beginAnchor = "";
	private String endAnchor = "";
	
	public SerializableConnection  () {
		
	}
	
	public UUID getBeginBlock() { return beginBlock; }
	public UUID getEndBlock()   { return endBlock; }
	public String getBeginAnchor(){ return beginAnchor; }
	public String getEndAnchor(){ return endAnchor; }
	
	public void setBeginBlock(UUID uuid) { beginBlock=uuid; }
	public void setEndBlock(UUID uuid)   { endBlock=uuid; }
	public void gsetBeginAnchor(String name){beginAnchor=name; }
	public void setEndAnchor(String name){endAnchor=name; }

}
