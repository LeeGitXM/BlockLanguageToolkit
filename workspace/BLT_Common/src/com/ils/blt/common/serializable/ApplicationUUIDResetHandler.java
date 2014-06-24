/**
 *   (c) 2013  ILS Automation. All rights reserved.
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
public class ApplicationUUIDResetHandler   {

	private final SerializableApplication application;
	private final Map<UUID,UUID> idLookup;      // Get new UUID from original
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	public ApplicationUUIDResetHandler(SerializableApplication sa) {
		this.application = sa;
		this.idLookup = new HashMap<UUID,UUID>();
	}

	/**
	 * Do it.
	 */
	public boolean convertUUIDs() {
		boolean success = false;
		// First the root application
		UUID original = application.getId();
		application.setId(UUID.randomUUID());
		if( original!=null ) idLookup.put(original, application.getId());
		
		// Now do the same for all families
		// - there are no parent references yet
		for(SerializableFamily fam:application.getFamilies()) {
			original = fam.getId();
			fam.setId(UUID.randomUUID());
			if( original!=null ) idLookup.put(original, fam.getId());
			
			// Now all the diagrams
			for(SerializableDiagram diagram:fam.getDiagrams()) {
				original = diagram.getId();
				diagram.setId(UUID.randomUUID());
				if( original!=null ) idLookup.put(original, fam.getId());
			}
		}
		
		return success;
	}
	
}
