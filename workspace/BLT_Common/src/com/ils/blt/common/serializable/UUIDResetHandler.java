/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.serializable;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  This class provides the utility function of resetting the UUIDs
 *  everywhere in a diagram. This is required for a diagram that is 
 *  cloned or imported. When a UUID is replaced, the old UUID is 
 *  also retained for use in the Gateway to transfer state information.
 */
public class UUIDResetHandler   {
	private final static String TAG = "UUIDResetHandler";
	private final LoggerEx log;
	private final SerializableDiagram diagram;
	private final HashMap<UUID,UUID> blockLookup;      // Get new UUID from original
	

	
	/**
	 * Initialize with instances of the classes to be controlled.
	 * @param sd the diagram
	 */
	public UUIDResetHandler(SerializableDiagram sd) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.diagram = sd;
		this.blockLookup = new HashMap<UUID,UUID>();
	}

	/**
	 * Do it.  (Note this will fix missing UUIDs).
	 * @return true if the conversion was a success
	 */
	public boolean convertUUIDs() {
		boolean success = true;
		UUID original = diagram.getId();
		diagram.setId(UUID.randomUUID());
		if( original!=null ) blockLookup.put(original, diagram.getId());
		
		// As we traverse the blocks, save off the UUIDs
		// so that we can look them up when we convert the 
		// connections.
		for( SerializableBlock sb:diagram.getBlocks()) {
			original = sb.getId();
			sb.setId(UUID.randomUUID());
			sb.setOriginalId(original);
			if (blockLookup.get(original) != null) {
				log.error("Duplicate block UUID found!! We are about to lose a link!! Original - :" + original.toString() + ": was linked to :" + blockLookup.get(original).toString() + ": now switching to :" + sb.getId().toString());
			}
			blockLookup.put(original, sb.getId());
			for(SerializableAnchor sa:sb.getAnchors()) {
				sa.setId(UUID.randomUUID());
				sa.setParentId(sb.getId());
			}
		}
		// Now the connections
		for(SerializableConnection sc:diagram.getConnections()) {
			UUID revised = null;
			// Start
			UUID id = sc.getBeginBlock();
			if( id!=null ) {  // Dangling
				revised = blockLookup.get(id);
				if( revised!=null ) {
					sc.setBeginBlock(revised);
				}
				else {
					log.warnf("%s: UUID lookup failed for begin block.", TAG);
					success = false;
				}
			}
			// End
			id = sc.getEndBlock();
			if( id!=null ) {  // Dangling
				revised = blockLookup.get(id);
				if( revised!=null ) {
					sc.setEndBlock(revised);
				}
				else {
					log.warnf("%s: UUID lookup failed for end block.", TAG);
				}
			}
			// And the anchors of the connections ...
			SerializableAnchorPoint sap = sc.getBeginAnchor();
			id = sap.getParentId();
			revised = blockLookup.get(id);
			if( revised!=null ) {
				sap.setParentId(revised);
			}
			else {
				log.warnf("%s: UUID lookup failed for begin anchor.", TAG);
				success = false;
			}
			sap = sc.getEndAnchor();
			id = sap.getParentId();
			revised = blockLookup.get(id);
			if( revised!=null ) {
				sap.setParentId(revised);
			}
			else {
				log.warnf("%s: UUID lookup failed for end anchor.", TAG);
				success = false;
			}
			
		}

		return success;
	}

	public HashMap<UUID,UUID> getBlockLookup() {
		return blockLookup;
	}
	
}
