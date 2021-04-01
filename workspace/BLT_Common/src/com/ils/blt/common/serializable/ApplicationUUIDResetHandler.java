/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.serializable;

import java.util.HashMap;
import java.util.UUID;

/**
 *  This class provides the utility function of resetting the UUIDs
 *  everywhere in an Application hierarchy. This is required for a tree that is 
 *  cloned or imported.
 */
public class ApplicationUUIDResetHandler   {

	private final SerializableApplication application;
	private final HashMap<UUID,UUID> idLookup;      // Get new UUID from original
	/**
	 * Initialize with instances of the classes to be controlled.
	 * @param sa the serializable application
	 */
	public ApplicationUUIDResetHandler(SerializableApplication sa) {
		this.application = sa;
		this.idLookup = new HashMap<UUID,UUID>();
	}

	/**
	 * Do it.
	 * @return true on success
	 */
	public boolean convertUUIDs(boolean goDeep) {
		boolean success = true;
		// First the root application
		UUID original = application.getId();
		application.setId(UUID.randomUUID());
		if( original!=null ) idLookup.put(original, application.getId());
		
		// Now do the same for all families
		// - there are no parent references yet
		
		
		// TODO EREIAM JH  HEY!!!!  Shouldn't this use FamilyUUIDResetHandler????
		

		if (goDeep) {
			for(SerializableFamily fam:application.getFamilies()) {
				FamilyUUIDResetHandler handler = new FamilyUUIDResetHandler(fam);
				handler.convertUUIDs(goDeep);
				idLookup.putAll(handler.getIdLookup());  // add the new blocks to the conversion table
			
//			original = fam.getId();
//			fam.setId(UUID.randomUUID());
//			if( original!=null ) idLookup.put(original, fam.getId());
//			
//			// Now all the diagrams
//			for(SerializableDiagram diagram:fam.getDiagrams()) {
//				UUIDResetHandler rhandler = new UUIDResetHandler(diagram);
//				rhandler.convertUUIDs();
//			}
			}
		}
		
		return success;
	}

	public HashMap<UUID,UUID> getIdLookup() {
		return idLookup;
	}
	
}
