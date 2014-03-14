package com.ils.blt.migration;


import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.ils.connection.ConnectionType;


/**
 * Implement a plain-old-java-object representing a connection.
 */
public class G2Connection {
	private AnchorDirection direction;
	private UUID block = null;
	private String port;

	private ConnectionType type;

	public G2Connection() {
	}
	
	public ConnectionType getType() {return type;}
	public AnchorDirection getDirection()   { return direction; }
	public UUID getBlock() { return block; }
	public String getPort() {return port;}
	public void setPort(String port) {this.port = port;}
	public void setType(ConnectionType connectionType) {this.type = connectionType;}
	public void setDirection(AnchorDirection t)   { direction=t; }
	public void setBlock(UUID id) { block = id; };


	


}
