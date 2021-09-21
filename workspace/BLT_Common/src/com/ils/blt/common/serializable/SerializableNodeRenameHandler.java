/**
 *   (c) 2013-2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.serializable;

import java.util.HashMap;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.StringPath;

/**
 *  This class updates resource paths of SerializableNode that have been copied or moved, and,
 *  in the special case of a diagram, handles  re-generating the UUIDs of the blocks within
 *  that diagram. When a UUID is replaced, the old UUID is 
 *  also retained for use in the Gateway to transfer state information.
 *   
 *  Do we reallyt serialize this? If not, put logger back.
 */
public class SerializableNodeRenameHandler   {
	private final static String CLSS = "SerializableNodeRenameHandler";
	private final HashMap<UUID,UUID> blockLookup;      // Save map of block UUIDs so we can deduce connections

	
	/**
	 * Initialize with instances of the classes to be controlled.
	 * @param sd the diagram
	 */
	public SerializableNodeRenameHandler() {
		this.blockLookup = new HashMap<UUID,UUID>();
	}
	
	/**
	 * This is the entry that is externally callable for all node types.
	 * @param root
	 * @param parent
	 */
	public void convertPaths(SerializableNode root,StringPath parent) {
		root.setParentPath(parent);
		
		if(root.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			SerializableApplication app = (SerializableApplication)root;
			for(SerializableFamily fam:app.getFamilies()) {
				convertPaths(fam,root.getPath());
			}
			for(SerializableFolder folder:app.getFolders()) {
				convertPaths(folder,root.getPath());
			}
		}
		else if(root.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
			SerializableFamily fam = (SerializableFamily)root;
			for(SerializableDiagram diag:fam.getDiagrams()) {
				convertDiagramPaths(diag,root.getPath());
			}
			for(SerializableFolder folder:fam.getFolders()) {
				convertPaths(folder,root.getPath());
			}
		}
		else if(root.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
			SerializableDiagram diag = (SerializableDiagram)root;
			convertDiagramPaths(diag,root.getPath());
		}
		else if(root.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
			SerializableFolder folder = (SerializableFolder)root;
			for(SerializableFamily fam:folder.getFamilies()) {
				convertPaths(fam,root.getPath());
			}
			for(SerializableFolder f:folder.getFolders()) {
				convertPaths(f,root.getPath());
			}
			for(SerializableDiagram diag:folder.getDiagrams()) {
				convertDiagramPaths(diag,root.getPath());
			}
		}
	}

	/**
	 * Convert block UUIDs and create connections. There is no recursion.
	 */
	private void convertDiagramPaths(SerializableDiagram diagram,StringPath parent) {
		blockLookup.clear();
		diagram.setParentPath(parent);
		
		// As we traverse the blocks, save off the UUIDs
		// so that we can look them up when we convert the 
		// connections.
		for( SerializableBlock sb:diagram.getBlocks()) {
			UUID original = sb.getId();
			sb.setId(UUID.randomUUID());
			sb.setOriginalId(original);
			if (blockLookup.get(original) != null) {
				//log.error("Duplicate block UUID found!! We are about to lose a link!! Original - :" + original.toString() + ": was linked to :" + blockLookup.get(original).toString() + ": now switching to :" + sb.getId().toString());
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
					//log.warnf("%s: UUID lookup failed for begin block.", CLSS);
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
					//log.warnf("%s: UUID lookup failed for end block.", CLSS);
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
				//log.warnf("%s: UUID lookup failed for begin anchor.", CLSS);
			}
			sap = sc.getEndAnchor();
			id = sap.getParentId();
			revised = blockLookup.get(id);
			if( revised!=null ) {
				sap.setParentId(revised);
			}
			else {
				//og.warnf("%s: UUID lookup failed for end anchor.", CLSS);
			}
			
		}
	}
}
