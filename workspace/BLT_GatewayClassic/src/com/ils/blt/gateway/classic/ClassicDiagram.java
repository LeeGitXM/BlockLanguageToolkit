/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.classic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.BasicDiagram;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;

/**
 * This diagram is the "model" that encapsulates the structure of the blocks and connections
 * of a ProcessDiagramView as viewed in the Designer.
 *  
 * This class provides answers to questions that the model control may ask about "what's next?".  
 */
public class ClassicDiagram extends BasicDiagram {
	
	private static final long serialVersionUID = 3667397875746466629L;
	private static String TAG = "ClassicDiagram";
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the serializable version of this object.
	 * @param parent 
	 */
	public ClassicDiagram(SerializableDiagram diagm,UUID parent,long projId) { 
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
			ProcessBlock pb = (ProcessBlock)blocks.get(id);
			if( pb==null ) {
				pb = blockFactory.blockFromSerializable(getSelf(),sb,getProjectId());
				if( pb!=null ) {
					// Set the proper timer
					if(DiagramState.ACTIVE.equals(state)) pb.setTimer(controller.getTimer());
					else if(DiagramState.ISOLATED.equals(state)) pb.setTimer(controller.getSecondaryTimer());
					pb.setProjectId(projectId);
					blocks.put(pb.getBlockId(), pb);
					log.debugf("%s.analyze: New block %s(%d)",TAG,pb.getName(),pb.hashCode());
					// If we create a block, then start any appropriate subscriptions
					for(BlockProperty bp:pb.getProperties()) {
						controller.startSubscription(pb,bp);
					}
					pb.start();
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
		for( CoreBlock cb:getDiagramBlocks()) {
			if( cb instanceof ProcessBlock ) {
				ProcessBlock block = (ProcessBlock)cb;
				if( !block.isReceiver() ) continue;
				SignalNotification sn = new SignalNotification(block,incoming.getValue());
				notifications.add(sn);
			}
		}
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
		// The blocks that delay start are those that propagate tag values.
		for(CoreBlock block:getDiagramBlocks() ) {
			if( block instanceof ProcessBlock && ((ProcessBlock)block).delayBlockStart() ) block.evaluate();
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
			// Check if we need to stop current subscriptions
			if( !DiagramState.DISABLED.equals(getState()) ) {
				stopSubscriptions();
			}
			// Stop blocks
			for(CoreBlock blk:blocks.values()) {
				blk.stop();
			}

			this.state = s;
			updateBlockTimers();

			if(!DiagramState.DISABLED.equals(getState()) ) {
				// The two-phase start is probably not necessary here
				// since we start the subscriptions after starting the blocks,
				// but we'll do it anyway for consistency
				for(CoreBlock blk:blocks.values()) {
					if( blk instanceof ProcessBlock && !((ProcessBlock)blk).delayBlockStart() ) blk.start();
				}
				for(CoreBlock blk:blocks.values()) {
					if( blk instanceof ProcessBlock && ((ProcessBlock)blk).delayBlockStart() ) blk.start();
				}
				restartSubscriptions();
			}
			// Fire diagram notification change
			controller.sendStateNotification(this.self.toString(), s.name());
		}
	}
	
	/**
	 * Loop through all blocks and start them
	 */
	public void startBlocks() {
		for( CoreBlock cb:getDiagramBlocks()) {
			if( cb instanceof ProcessBlock && !((ProcessBlock)cb).delayBlockStart() ) cb.start();
		}
		for( CoreBlock cb:getDiagramBlocks()) {
			if( cb instanceof ProcessBlock && ((ProcessBlock)cb).delayBlockStart() ) cb.start();
		}
	}
	
	@Override
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = super.toResourceDescriptor();
		descriptor.setType(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE);
		return descriptor;
	}
	/**
	 * Loop through the diagram's blocks setting the timer appropriate
	 * to the diagram's state.
	 */
	@Override
	public void updateBlockTimers() {
		// Set the proper timer
		WatchdogTimer timer = controller.getTimer();
		if( DiagramState.ISOLATED.equals(getState())) timer = controller.getSecondaryTimer();
		for(CoreBlock blk:blocks.values()) {
			if( blk instanceof ProcessBlock ) ((ProcessBlock)blk).setTimer(timer);
		}
	}
	

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
			if( cb!=null && cb instanceof ProcessBlock ) {
				ProcessBlock pb = (ProcessBlock)cb;
				log.debugf("%s.updateProperties: Update block %s",TAG,pb.getName());
				boolean hasChanged = false;
				// Stop old subscriptions ONLY if the property changed, or no longer exists
				// NOTE: The blockFactory update will take care of values. We're just worried about subscriptions
				for( BlockProperty newProp:sb.getProperties() ) {
					BlockProperty prop = pb.getProperty(newProp.getName());
					if( prop!=null ) {
						// See if the binding changed.
						if( !prop.getBindingType().equals(newProp.getBindingType()) ) {
							hasChanged = true;
							pb.stop();
							// If the binding has changed - fix subscriptions.
							if(prop.getBindingType().equals(BindingType.TAG_MONITOR) ||
							   prop.getBindingType().equals(BindingType.TAG_READ)    ||
							   prop.getBindingType().equals(BindingType.TAG_READWRITE) ||
							   prop.getBindingType().equals(BindingType.TAG_WRITE)) controller.removeSubscription(pb,prop);
							prop.setBindingType(newProp.getBindingType());
							prop.setBinding(newProp.getBinding());
							if(prop.getBindingType().equals(BindingType.TAG_MONITOR) ||
							   prop.getBindingType().equals(BindingType.TAG_READ)    ||
							   prop.getBindingType().equals(BindingType.TAG_READWRITE) ||
							   prop.getBindingType().equals(BindingType.TAG_WRITE)) {
								controller.startSubscription(pb,prop);
								// If the new binding is a tag write - do the write.
								if( !pb.isLocked() && 
									(prop.getBindingType().equals(BindingType.TAG_READWRITE) ||
											prop.getBindingType().equals(BindingType.TAG_WRITE))	   ) {
										controller.updateTag(pb.getParentId(),prop.getBinding(), new BasicQualifiedValue(newProp.getValue()));
								}
							}
						}
						else if( !prop.getBindingType().equals(BindingType.NONE) &&
								 !prop.getBindingType().equals(BindingType.OPTION) &&
								 !prop.getBinding().equals(newProp.getBinding()) ) {
							// Same type, new binding target.
							hasChanged = true;
							pb.stop();
							controller.removeSubscription(pb, prop);
							prop.setBinding(newProp.getBinding());
							controller.startSubscription(pb,prop);
							// If the new binding is a tag write - do the write.
							if( !pb.isLocked() && 
									(prop.getBindingType().equals(BindingType.TAG_READWRITE) ||
								     prop.getBindingType().equals(BindingType.TAG_WRITE))	   ) {
								controller.updateTag(pb.getParentId(),prop.getBinding(), new BasicQualifiedValue(newProp.getValue()));
							}	
						}
					}
				}
				blockFactory.updateBlockFromSerializable(pb,sb);
				if( hasChanged ) {
					pb.start();
				}
			}
			else {
				log.errorf("%s.updateProperties: ERROR, block %s missing in diagram %s ",TAG,sb.getName(),diagram.getName());
			}
		}
	}	
}
