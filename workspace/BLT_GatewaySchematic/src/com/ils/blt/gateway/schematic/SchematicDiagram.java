/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.schematic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.BasicDiagram;
import com.ils.sblock.Input;
import com.ils.sblock.SchematicBlock;

/**
 * This diagram is the "model" that encapsulates the structure of the blocks and connections
 * of a ProcessDiagramView as viewed in the Designer.
 *  
 * This class provides answers to questions that the model control may ask about "what's next?".  
 */
public class SchematicDiagram extends BasicDiagram {
	
	private static final long serialVersionUID = 3667397875746466629L;
	private static String TAG = "ClassicDiagram";
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the serializable version of this object.
	 * @param parent 
	 */
	public SchematicDiagram(SerializableDiagram diagm,UUID parent,long projId) { 
		super(diagm,parent,projId);
	}
	/**
	 * Clone blocks from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any blocks that currently
	 * exist. Newly created blocks are started.
	 * 
	 * @param diagram the serializable template of this object.
	 */
	@Override
	public void createBlocks(SerializableDiagram diagram) {
		BlockFactory blockFactory = BlockFactory.getInstance();

		// Update the blocks - we've already deleted any not present in the new
		SerializableBlock[] sblks = diagram.getBlocks();
		for( SerializableBlock sb:sblks ) {
			UUID id = sb.getId();
			SchematicBlock pb = (SchematicBlock)blocks.get(id);
			if( pb==null ) {
				pb = blockFactory.blockFromSerializable(getSelf(),sb,getProjectId());
				if( pb!=null ) {
					pb.setProjectId(projectId);
					blocks.put(pb.getBlockId(), pb);
					log.debugf("%s.analyze: New block %s(%d)",TAG,pb.getName(),pb.hashCode());
				}
				else {
					log.errorf("%s.analyze: ERROR, diagram %s failed to instantiate block of type %s",TAG,diagram.getName(),sb.getClassName());
				}
			}
		}
	}
	
	/**
	 * We have just received a request to broadcast a signal. Determine which blocks are to be notified,
	 * and create notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no appropriate blocks were found.
	 * @param new notification to broadcast
	 * @return a new value notification for the receiving block(s)
	 */
	@Override
	public Collection<SignalNotification> getBroadcastNotifications(BroadcastNotification incoming) {
		
		Collection<SignalNotification>notifications = new ArrayList<SignalNotification>();
		return notifications;
	}

	/**
	 * Reset all blocks in the diagram, then evaluate the inputs.
	 */
	@Override
	public void reset() {
		for(CoreBlock block:getDiagramBlocks() ) {
			block.reset();
		}
	}
	
	/**
	 * Set the state of the diagram. Note that the state does not affect the activity 
	 * of blocks within the diagram. It only affects the way that block results are 
	 * propagated (or not) and whether or not subscriptions are in effect.
	 * 
	 * If the new state is ISOLATED, stop all blocks, set the state and re-start. 
	 * This is necessary to allow a swap-out of timers and tag providers. During
	 * this time, set the state to DISABLED to prevent propagation of new tag values.
	 * Likewise if the state was ISOLATED, perform the same sequence.
	 * 
	 * @param s the new state
	 */
	@Override
	public void setState(DiagramState s) {
		// Do nothing if there is no change
		if( !s.equals(getState())) {
			this.state = s;
		}
	}
	
	/**
	 * Loop through blocks and cause the Inputs to evaluate
	 */
	public void startBlocks() {
		for( CoreBlock cb:getDiagramBlocks()) {
			if( cb instanceof Input ) cb.start();
		}
	}
	
	@Override
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = super.toResourceDescriptor();
		descriptor.setType(BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE);
		return descriptor;
	}
	/**
	 * This does not apply to a schematic diagram.
	 */
	@Override
	public void updateBlockTimers() {}

	/**
	 * Compare blocks in the serializable version to the current. Update and adjust subscriptions where necessary.
	 * 
	 * @param diagram the serializable template of this object.
	 */
	public void updateProperties(SerializableDiagram diagram) {
		BlockFactory blockFactory = BlockFactory.getInstance();
		SerializableBlock[] sblks = diagram.getBlocks();
		for( SerializableBlock sb:sblks ) {
			UUID id = sb.getId();
			CoreBlock cb = blocks.get(id);
			if( cb!=null && cb instanceof SchematicBlock ) {
				SchematicBlock pb = (SchematicBlock)cb;
				blockFactory.updateBlockFromSerializable(pb,sb);
			}
			else {
				log.errorf("%s.updateProperties: ERROR, block %s missing in diagram %s ",TAG,sb.getName(),diagram.getName());
			}
		}
	}	
}
