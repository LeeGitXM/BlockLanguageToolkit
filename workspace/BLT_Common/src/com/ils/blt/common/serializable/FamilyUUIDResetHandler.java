/**
 *   (c) 2018  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.serializable;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;



/**
 *  This class provides the utility function of resetting the UUIDs
 *  everywhere in an Application hierarchy. This is required for a tree that is 
 *  cloned or imported.
 */
public class FamilyUUIDResetHandler   {

	private final SerializableFamily family;
	private final HashMap<UUID,UUID> idLookup;      // Get new UUID from original
	/**
	 * Initialize with instances of the classes to be controlled.
	 * @param sf the serializable family
	 */
	public FamilyUUIDResetHandler(SerializableFamily sf) {
		this.family = sf;
		this.idLookup = new HashMap<UUID,UUID>();
	}

	/**
	 * Do it.
	 * @return true on success
	 */
	public boolean convertUUIDs(boolean goDeep) {
		boolean success = true;
		// First the root family
		UUID original = family.getId();
		family.setId(UUID.randomUUID());
		if( original!=null ) idLookup.put(original, family.getId());
		
		// Now do the same for all diagrams
		// - there are no parent references yet
		// Now all the diagrams
		if (goDeep) {
			for(SerializableDiagram diagram:family.getDiagrams()) {
				UUIDResetHandler rhandler = new UUIDResetHandler(diagram);
				rhandler.convertUUIDs();
				idLookup.putAll(rhandler.getBlockLookup());  // add the new blocks to the conversion table
			}
		}
		
		return success;
	}

	public HashMap<UUID,UUID> getIdLookup() {
		return idLookup;
	}
	
}
