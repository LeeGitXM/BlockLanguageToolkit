package com.ils.blt.common.serializable;

import java.util.UUID;

/**
 * Implement a plain-old-java-object representing a connection
 * that is serializable via a XML or JSON serializer.
 */
public class SerializableConnection {
	private UUID beginBlock = null;
	private UUID endBlock = null;
	private SerializableAnchorPoint beginAnchor = null;
	private SerializableAnchorPoint endAnchor = null;
	
	public SerializableConnection  () {
	}
	
	public UUID getBeginBlock() { return beginBlock; }
	public UUID getEndBlock()   { return endBlock; }
	public SerializableAnchorPoint getBeginAnchor(){ return beginAnchor; }
	public SerializableAnchorPoint getEndAnchor(){ return endAnchor; }
	
	public void setBeginBlock(UUID uuid) { beginBlock=uuid; }
	public void setEndBlock(UUID uuid)   { endBlock=uuid; }
	public void setBeginAnchor(SerializableAnchorPoint anchor){beginAnchor=anchor; }
	public void setEndAnchor(SerializableAnchorPoint anchor){endAnchor=anchor; }

}
