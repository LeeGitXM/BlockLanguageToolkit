package com.ils.blt.migration;


import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.ils.connection.ConnectionType;


/**
 * Implement a plain-old-java-object representing a connection.
 * This represents a G2 stub. It is an end-piece of a connection.
 * The block-name is unique within a G2 instance. 
 * The UUID here doesn't help, as it differs on each end.
 */
public class G2Anchor {
	private final static String TAG = "G2Anchor";
	private String blockClass;
	private String blockName;
	private String direction;
	private String port;
	private String type;
	private String uuid;

	public G2Anchor() {
	}

	public AnchorDirection getAnchorDirection()   { 
		AnchorDirection result = AnchorDirection.INCOMING;
		if( direction.equalsIgnoreCase("IN") ||
			direction.equalsIgnoreCase("INPUT")	) {
			result = AnchorDirection.INCOMING;
		}
		else if( direction.equalsIgnoreCase("OUT") ||
				 direction.equalsIgnoreCase("OUTPUT")) {
			result = AnchorDirection.OUTGOING;
		}
		else {
			System.err.println(String.format("%s.getAnchorDirection: Unrecognized direction (%s)",TAG,direction));
		}
		return result; 
	}

	public String getBlockClass() {return blockClass;}
	public String getBlockName() {return blockName;}
	public ConnectionType getConnectionType() {
		ConnectionType result = ConnectionType.DATA;
		if( type.equalsIgnoreCase("GDL-DATA-PATH")) {
			result = ConnectionType.DATA;
		}
		else {
			System.err.println(String.format("%s.getConnectionType: Unrecognized connection type (%s)",TAG,type));
		}
		return result;
	}
	public String getDirection() {return direction;}
	public String getPort() {return port;}
	public String getType() {return type;}
	public String getUuid() {return uuid;}
	public void setBlockClass(String blockClass) {this.blockClass = blockClass;}
	public void setBlockName(String blockName) {this.blockName = blockName;}
	public void setDirection(String d)   { this.direction=d; }
	public void setPort(String port) {this.port = port;}
	public void setType(String connectionType) {this.type = connectionType;}
	public void setUuid(String uuid) {this.uuid = uuid;};


	


}