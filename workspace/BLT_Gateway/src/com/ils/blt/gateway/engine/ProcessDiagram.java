/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.connection.ProcessConnection;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This diagram is the "model" that encapsulates the structure of the blocks and connections
 * of a ProcessDiagramView as viewed in the Designer.
 *  
 * This class provides answers to questions that the model control may ask about "what's next?".  
 * 
 *  The document is constant for the life of this instance.
 */
public class ProcessDiagram extends ProcessNode {
	private static final long serialVersionUID = 3557397875746466629L;
	private static String TAG = "ProcessDiagram";
	private boolean valid = false;
	protected final Map<UUID,ProcessBlock> blocks;
	private final Map<ConnectionKey,ProcessConnection> connectionMap;            // Key by connection number
	protected final Map<BlockPort,List<ProcessConnection>> incomingConnections;  // Key by downstream block:port
	protected final Map<BlockPort,List<ProcessConnection>> outgoingConnections;  // Key by upstream block:port
	private DiagramState state = DiagramState.UNSET;                             // So that new state will be a change
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the serializable version of this object.
	 * @param parent 
	 */
	public ProcessDiagram(SerializableDiagram diagm,UUID parent,long projId) { 
		super(diagm.getName(),parent,diagm.getId());
		this.state = diagm.getState();
		this.resourceId = diagm.getResourceId();
		this.projectId = projId;
		blocks = new HashMap<UUID,ProcessBlock>();
		connectionMap = new HashMap<ConnectionKey,ProcessConnection>();
		incomingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
		outgoingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
	}

	public ProcessBlock getBlock(UUID id) { return blocks.get(id); }
	// For now we just do a linear search
	public ProcessBlock getBlockByName(String name) { 
		ProcessBlock result = null;
		for(ProcessBlock blk:getProcessBlocks()) {
			if(blk.getName().equals(name)) {
				result = blk;
				break;
			}
		}
		return result;
	}
	public Collection<ProcessBlock> getProcessBlocks() { return blocks.values(); }

	
	/**
	 * Prepare for an update from a newly deserialized node.
	 */
	public void clearConnections() {
		connectionMap.clear();
		incomingConnections.clear();
		outgoingConnections.clear();
	}
	/**
	 * Remove blocks in this diagram that are NOT in the 
	 * supplied list. Kill any tag subscriptions associated with those blocks.
	 */
	public void removeUnusedBlocks(SerializableBlock[] newBlocks) {
		List<UUID> uuids = new ArrayList<>();
		for(SerializableBlock sb:newBlocks) {
			uuids.add(sb.getId());
		}
		List<ProcessBlock> blocksToRemove = new ArrayList<>();
		for(ProcessBlock oldBlock:blocks.values()) {
			if(!uuids.contains(oldBlock.getBlockId()) ) blocksToRemove.add(oldBlock);
		}
		for(ProcessBlock oldBlock:blocksToRemove) {
			blocks.remove(oldBlock.getBlockId());
			for(BlockProperty prop:oldBlock.getProperties()) {
				controller.removeSubscription(oldBlock, prop);
			}
			oldBlock.stop();
			log.infof("%s.removeBlocksFromList: decommissioned %s (%d)",TAG,oldBlock.getName(),oldBlock.hashCode());
		}
	}
	
	/**
	 * Clone blocks from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any blocks that currently
	 * exist. Newly created blocks are started.
	 * 
	 * @param diagram the serializable template of this object.
	 */
	public void createBlocks(SerializableDiagram diagram) {
		BlockFactory blockFactory = BlockFactory.getInstance();

		// Update the blocks - we've already deleted any not present in the new
		SerializableBlock[] sblks = diagram.getBlocks();
		for( SerializableBlock sb:sblks ) {
			UUID id = sb.getId();
			ProcessBlock pb = blocks.get(id);
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
	 * Clone connections from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any connections that currently
	 * exist.
	 * 
	 * @param diagram the serializable template of this object.
	 */
	public void updateConnections(SerializableDiagram diagram) {
		ConnectionFactory connectionFactory = ConnectionFactory.getInstance();
		// Update the connections
		SerializableConnection[] scxns = diagram.getConnections();
		for( SerializableConnection sc:scxns ) {
			if( validConnection(sc) ) {
				ConnectionKey cxnkey = new ConnectionKey(sc.getBeginBlock().toString(),sc.getBeginAnchor().getId().toString(),
						sc.getEndBlock().toString(),sc.getEndAnchor().getId().toString());
				ProcessConnection pc = connectionMap.get(cxnkey);
				if( pc==null ) {
					pc = connectionFactory.connectionFromSerializable(sc);
				}
				else {
					connectionFactory.updateConnectionFromSerializable(pc,sc);
				}
				// Add the connection to the map. The block-port is for the upstream block
				ProcessBlock upstreamBlock = blocks.get(pc.getSource());
				if( upstreamBlock!=null ) {
					BlockPort key = new BlockPort(upstreamBlock,pc.getUpstreamPortName());
					List<ProcessConnection> connections = outgoingConnections.get(key);
					if( connections==null ) {
						connections = new ArrayList<ProcessConnection>();
						outgoingConnections.put(key, connections);
						log.tracef("%s.updateConnections: mapping connection from %s:%s",TAG,upstreamBlock.getBlockId().toString(),pc.getUpstreamPortName());
					}
					if( !connections.contains(pc) ) connections.add(pc);
				}
				else {
					log.warnf("%s.updateConnections: Source block (%s) not found for connection",TAG,pc.getSource().toString());
				}
				ProcessBlock downstreamBlock = blocks.get(pc.getTarget());
				if( downstreamBlock!=null && pc.getDownstreamPortName()!=null) {
					BlockPort key = new BlockPort(downstreamBlock,pc.getDownstreamPortName());
					List<ProcessConnection> connections = incomingConnections.get(key);
					if( connections==null ) {
						connections = new ArrayList<ProcessConnection>();
						incomingConnections.put(key, connections);
					}
					if( !connections.contains(pc) ) connections.add(pc);
				}
				else {
					log.warnf("%s.updateConnections: Target block (%s) not found for connection",TAG,pc.getTarget().toString());
				}
			}
			else {
				log.warnf("%s.updateConnections: %s has invalid serialized connection (%s)",TAG,diagram.getName(),invalidConnectionReason(sc));
			}

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
			ProcessBlock pb = blocks.get(id);
			if( pb!=null ) {
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
	
	/**
	 * @return a Connection from the diagram given its id.
	 */
	public Connection getConnection(String id) { return connectionMap.get(id); }

	
	/**
	 * We have just received a request to broadcast a signal. Determine which blocks are to be notified,
	 * and create notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no appropriate blocks were found.
	 * @param new notification to broadcast
	 * @return a new value notification for the receiving block(s)
	 */
	public Collection<SignalNotification> getBroadcastNotifications(BroadcastNotification incoming) {
		
		Collection<SignalNotification>notifications = new ArrayList<SignalNotification>();
		for( ProcessBlock block:getProcessBlocks()) {
			if( !block.isReceiver() ) continue;
			SignalNotification sn = new SignalNotification(block,incoming.getSignal());
			notifications.add(sn);
		}
		return notifications;
	}
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the output(s) of the specified block.
	 */
	public List<ProcessBlock> getDownstreamBlocks(ProcessBlock root) {
		List<ProcessBlock> downstream = new ArrayList<>();
		for( AnchorPrototype ap:root.getAnchors() ) {
			if( ap.getAnchorDirection().equals(AnchorDirection.OUTGOING)) {
				String port = ap.getName();
				BlockPort key = new BlockPort(root,port);
				List<ProcessConnection> cxns = outgoingConnections.get(key);
				if( cxns!=null ) {
					for(ProcessConnection cxn:cxns) {
						UUID blockId = cxn.getTarget();
						downstream.add(blocks.get(blockId));
					}
				}		
			}
		}
		return downstream;
	}
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the output(s) of the specified block.
	 */
	public List<ProcessBlock> getUpstreamBlocks(ProcessBlock root) {
		List<ProcessBlock> upstream = new ArrayList<>();
		for( AnchorPrototype ap:root.getAnchors() ) {
			if( ap.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
				String port = ap.getName();
				BlockPort key = new BlockPort(root,port);
				List<ProcessConnection> cxns = incomingConnections.get(key);
				if( cxns!=null ) {
					for(ProcessConnection cxn:cxns) {
						UUID blockId = cxn.getSource();
						upstream.add(blocks.get(blockId));
					}
				}		
			}
		}
		return upstream;
	}
	/**
	 * We have just received a notification of a value change. Determine which blocks are connected downstream,
	 * and create notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no downstream connection.
	 * @param new value notification of an incoming change
	 * @return a new value notification for the downstream block(s)
	 */
	public Collection<IncomingNotification> getOutgoingNotifications(OutgoingNotification incoming) {
		ProcessBlock block = incoming.getBlock();
		String port = incoming.getPort();
		QualifiedValue value = incoming.getValue();
		
		Collection<IncomingNotification>notifications = new ArrayList<IncomingNotification>();
		BlockPort key = new BlockPort(block,port);
		if( outgoingConnections.get(key)!=null ) {
			List<ProcessConnection> cxns = new ArrayList<>(outgoingConnections.get(key));
			for(ProcessConnection cxn:cxns) {
				UUID blockId = cxn.getTarget();
				ProcessBlock blk = blocks.get(blockId);
				if( blk!=null ) {
					IncomingNotification vcn = new IncomingNotification(cxn,value);
					notifications.add(vcn);
				}
				else {
					log.warnf("%s.getOutgoingNotifications: Target block %s not found for connection",TAG,blockId.toString());
				}
			}
		}
		else {
			log.debugf("%s.getOutgoingNotifications: no connections found for %s:%s",TAG,block.getBlockId().toString(),port);
		}
		return notifications;
	}
	
	public DiagramState getState() {return state;}
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
	public void setState(DiagramState s) {
		// Do nothing if there is no change
		if( !s.equals(getState())) {
			// Check if we need to stop current subscriptions
			if( !DiagramState.DISABLED.equals(getState()) ) {
				stopSubscriptions();
			}
			// Stop blocks
			for(ProcessBlock blk:blocks.values()) {
				blk.stop();
			}

			this.state = s;
			updateBlockTimers();

			if(!DiagramState.DISABLED.equals(getState()) ) {
				// The two-phase start is probably not necessary here
				// since we start the subscriptions after starting the blocks,
				// but we'll do it anyway for consistency
				for(ProcessBlock blk:blocks.values()) {
					if( !blk.delayBlockStart() ) blk.start();
				}
				for(ProcessBlock blk:blocks.values()) {
					if( blk.delayBlockStart() ) blk.start();
				}
				restartSubscriptions();
			}
			// Fire diagram notification change
			controller.sendStateNotification(this.self.toString(), s.name());
		}
	}
	/**
	 * Report on whether or not the DOM contained more than one connected node.
	 */
	public boolean isValid() { return valid; }
	@Override
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = super.toResourceDescriptor();
		descriptor.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
		return descriptor;
	}
	/**
	 * Loop through the diagram's blocks setting the timer appropriate
	 * to the diagram's state.
	 */
	public void updateBlockTimers() {
		// Set the proper timer
		WatchdogTimer timer = controller.getTimer();
		if( DiagramState.ISOLATED.equals(getState())) timer = controller.getSecondaryTimer();
		for(ProcessBlock blk:blocks.values()) {
			blk.setTimer(timer);
		}
	}
	/**
	 * Make sure all components of a serializable connection are present. 
	 * If not, the connection will be rejected.
	 * @param sc
	 * @return validity flag
	 */
	private boolean validConnection(SerializableConnection sc) {
		boolean validCxn = false;
		if( sc.getBeginBlock()    != null &&
			sc.getBeginAnchor()   !=null  &&
			sc.getBeginAnchor().getId()!=null &&
			sc.getEndBlock()      !=null &&
			sc.getEndAnchor()     !=null &&
			sc.getEndAnchor().getId()!=null   ) {
			validCxn = true;
		}
		return validCxn;
	}
	/**
	 * @param sc
	 * @return a description of why the proposed connection is invalid.
	 */
	private String invalidConnectionReason(SerializableConnection sc) {
		String reason = "No reason";
		if( sc.getBeginBlock()          == null )      reason = "No begin block";
		else if(sc.getBeginAnchor()     == null )      reason = "No begin anchor";
		else if(sc.getBeginAnchor().getId()==null)     reason = "Begin anchor has no Id";
		else if(sc.getEndBlock()        == null )      reason = "No end block";
		else if(sc.getEndAnchor()       == null )      reason = "No end anchor";
		else if(sc.getEndAnchor().getId()==null )      reason = "End anchor has no Id";
		return reason;
	}
	// This should only be called on a new diagram, or on new blocks for an existing
	// diagram. Note that "starting" a subscription on an existing property *should* 
	// do nothing.
	public void startSubscriptions() {
		//log.infof("%s.startSubscriptions: ...%d:%s",TAG,projectId,getName());
		for( ProcessBlock pb:getProcessBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				controller.startSubscription(pb,bp);
			}
		}
	}
	
	// This is only called on a diagram state change. Here we temporarily set the
	// diagram to DISABLED in order to suppress tag updates due to new bindings.
	private void restartSubscriptions() {
		//log.infof("%s.restartSubscriptions: ...%d:%s",TAG,projectId,getName());
		DiagramState current = this.state;
		this.state = DiagramState.DISABLED;
		for( ProcessBlock pb:getProcessBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				controller.restartSubscription(this,pb,bp,current);
			}
		}
		//log.infof("%s.restartSubscriptions: ... %s complete",TAG,getName());
		this.state = current;
	}
	private void stopSubscriptions() {
		log.infof("%s.stopSubscriptions: ...%d:%s",TAG,projectId,getName());
		for( ProcessBlock pb:getProcessBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				controller.removeSubscription(pb,bp);
			}
		}
	}
	/**
	 * Class for keyed storage of downstream block,port for a connection.
	 */
	protected class BlockPort {
		private final String port;
		private final ProcessBlock block;
		public BlockPort(ProcessBlock block,String port) {
			this.port = port;
			this.block = block;
		}
		public String getPort() { return this.port; }
		public ProcessBlock getBlock() { return this.block; }
		
		// So that class may be used as a map key
		// Same blockId and port is sufficient to prove equality
		@Override
		public boolean equals(Object arg) {
			boolean result = false;
			if( arg instanceof BlockPort) {
				BlockPort that = (BlockPort)arg;
				if( this.block.getBlockId().equals(that.getBlock().getBlockId() ) &&
					this.port.equals(that.getPort())   ) {
					result = true;
				}
			}
			return result;
		}
		@Override
		public int hashCode() {
			return this.block.getBlockId().hashCode()+port.hashCode();
		}
	}
	/**
	 * The key is the block Ids and port names for both source and target.
	 */
	protected class ConnectionKey {
		private final String sourceBlock;
		private final String sourcePort;
		private final String targetBlock;
		private final String targetPort;
		
		public ConnectionKey(String sb,String sp,String tb,String tp) {
			this.sourceBlock = sb;
			this.sourcePort = sp;
			this.targetBlock = tb;
			this.targetPort = tp;
		}
		
		public String getSourcePort()  { return this.sourcePort; }
		public String getSourceBlock() { return this.sourceBlock; }
		public String getTargetPort()  { return this.targetPort; }
		public String getTargetBlock() { return this.targetBlock; }
		
		// So that class may be used as a map key
		// Same source and target ports are sufficient to prove equality
		@Override
		public boolean equals(Object arg) {
			boolean result = false;
			if( arg instanceof ConnectionKey) {
				ConnectionKey that = (ConnectionKey)arg;
				if( this.sourceBlock.equals(that.getSourceBlock()) &&
					this.sourcePort.equals(that.getSourcePort())   &&
					this.targetBlock.equals(that.getTargetBlock()) &&
					this.targetPort.equals(that.getTargetPort())      ) {
					result = true;
				}
			}
			return result;
		}
		@Override
		public int hashCode() {
			return this.sourceBlock.hashCode()+this.sourcePort.hashCode()-this.targetBlock.hashCode()-this.targetPort.hashCode();
		}
	}
	
}
