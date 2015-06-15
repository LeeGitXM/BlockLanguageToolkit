/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

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
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.CoreBlock;
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
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ConnectionFactory;
import com.ils.blt.gateway.engine.ProcessNode;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This diagram is the "model" that encapsulates the structure of the blocks and connections
 * of a ProcessDiagramView as viewed in the Designer.
 *  
 * This class provides answers to questions that the model control may ask about "what's next?".  
 * 
 *  The document is constant for the life of this instance.
 */
public abstract class BasicDiagram extends ProcessNode {
	private static final long serialVersionUID = 3557397875746466629L;
	private static String TAG = "BasicDiagram";
	private boolean valid = false;
	protected final Map<UUID,CoreBlock> blocks;
	private final Map<ConnectionKey,ProcessConnection> connectionMap;            // Key by connection number
	protected final Map<BlockPort,List<ProcessConnection>> incomingConnections;  // Key by downstream block:port
	protected final Map<BlockPort,List<ProcessConnection>> outgoingConnections;  // Key by upstream block:port
	
	protected DiagramState state = DiagramState.UNSET;                             // So that new state will be a change
	protected final BlockExecutionController controller = BlockExecutionController.getInstance();
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the serializable version of this object.
	 * @param parent 
	 */
	public BasicDiagram(SerializableDiagram diagm,UUID parent,long projId) { 
		super(diagm.getName(),parent,diagm.getId());
		this.state = diagm.getState();
		this.resourceId = diagm.getResourceId();
		this.projectId = projId;
		blocks = new HashMap<UUID,CoreBlock>();
		connectionMap = new HashMap<ConnectionKey,ProcessConnection>();
		incomingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
		outgoingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
	}
	/**
	 * Prepare for an update from a newly deserialized node.
	 */
	public void clearConnections() {
		connectionMap.clear();
		incomingConnections.clear();
		outgoingConnections.clear();
	}

	
	/**
	 * Clone blocks from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any blocks that currently
	 * exist. Newly created blocks are started.
	 * 
	 * @param diagram the serializable template of this object.
	 */
	public abstract void createBlocks(SerializableDiagram diagram);
	public CoreBlock getBlock(UUID id) { return blocks.get(id); }
	
	// For now we just do a linear search
	public CoreBlock getBlockByName(String name) { 
		CoreBlock result = null;
		for(CoreBlock blk:getDiagramBlocks()) {
			if(blk.getName().equals(name)) {
				result = blk;
				break;
			}
		}
		return result;
	}

	/**
	 * We have just received a request to broadcast a signal. Determine which blocks are to be notified,
	 * and create notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no appropriate blocks were found.
	 * @param new notification to broadcast
	 * @return a new value notification for the receiving block(s)
	 */
	public abstract Collection<SignalNotification> getBroadcastNotifications(BroadcastNotification incoming);
	
	/**
	 * @return a Connection from the diagram given its id.
	 */
	public Connection getConnection(String id) { return connectionMap.get(id); }
	
	public Collection<CoreBlock> getDiagramBlocks() { return blocks.values(); }

	
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the output(s) of the specified block.
	 */
	public List<CoreBlock> getDownstreamBlocks(CoreBlock root) {
		List<CoreBlock> downstream = new ArrayList<>();
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
	 * We have just received a notification of a value change. Determine which blocks are connected downstream,
	 * and create notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no downstream connection.
	 * @param new value notification of an incoming change
	 * @return a new value notification for the downstream block(s)
	 */
	public Collection<IncomingNotification> getOutgoingNotifications(OutgoingNotification incoming) {
		CoreBlock block = incoming.getBlock();
		String port = incoming.getPort();
		QualifiedValue value = incoming.getValue();
		
		Collection<IncomingNotification>notifications = new ArrayList<IncomingNotification>();
		BlockPort key = new BlockPort(block,port);
		List<ProcessConnection> cxns = outgoingConnections.get(key);
		if( cxns!=null ) {
			for(ProcessConnection cxn:cxns) {
				UUID blockId = cxn.getTarget();
				CoreBlock blk = blocks.get(blockId);
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
	 * @param root the subject block
	 * @return a list of blocks connected to the output(s) of the specified block.
	 */
	public List<CoreBlock> getUpstreamBlocks(CoreBlock root) {
		List<CoreBlock> upstream = new ArrayList<>();
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
	 * @param sc
	 * @return a description of why the proposed connection is invalid.
	 */
	protected String invalidConnectionReason(SerializableConnection sc) {
		String reason = "No reason";
		if( sc.getBeginBlock()          == null )      reason = "No begin block";
		else if(sc.getBeginAnchor()     == null )      reason = "No begin anchor";
		else if(sc.getBeginAnchor().getId()==null)     reason = "Begin anchor has no Id";
		else if(sc.getEndBlock()        == null )      reason = "No end block";
		else if(sc.getEndAnchor()       == null )      reason = "No end anchor";
		else if(sc.getEndAnchor().getId()==null )      reason = "End anchor has no Id";
		return reason;
	}
	/**
	 * Report on whether or not the DOM contained more than one connected node.
	 */
	public boolean isValid() { return valid; }
	/**
	 * Remove blocks in this diagram that are NOT in the 
	 * supplied list. Kill any tag subscriptions associated with those blocks.
	 */
	public void removeUnusedBlocks(SerializableBlock[] newBlocks) {
		List<UUID> uuids = new ArrayList<>();
		for(SerializableBlock sb:newBlocks) {
			uuids.add(sb.getId());
		}
		List<CoreBlock> blocksToRemove = new ArrayList<>();
		for(CoreBlock oldBlock:blocks.values()) {
			if(!uuids.contains(oldBlock.getBlockId()) ) blocksToRemove.add(oldBlock);
		}
		for(CoreBlock oldBlock:blocksToRemove) {
			blocks.remove(oldBlock.getBlockId());
			for(BlockProperty prop:oldBlock.getProperties()) {
				controller.removeSubscription(oldBlock, prop);
			}
			oldBlock.stop();
			log.infof("%s.removeBlocksFromList: decommissioned %s (%d)",TAG,oldBlock.getName(),oldBlock.hashCode());
		}
	}
	/**
	 * The base method resets all block in the diagram.
	 */
	public void reset() {
		for(CoreBlock block:getDiagramBlocks() ) {
			block.reset();
		}
	}
	// This is only called on a diagram state change. Here we temporarily set the
	// diagram to DISABLED in order to suppress tag updates due to new bindings.
	protected void restartSubscriptions() {
		//log.infof("%s.restartSubscriptions: ...%d:%s",TAG,projectId,getName());
		DiagramState current = this.state;
		this.state = DiagramState.DISABLED;
		for( CoreBlock pb:getDiagramBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				controller.restartSubscription(this,pb,bp,current);
			}
		}
		//log.infof("%s.restartSubscriptions: ... %s complete",TAG,getName());
		this.state = current;
	}
	/**
	 * Set the state of the diagram. Note that the state does not affect the activity 
	 * of blocks within the diagram. It only affects the way that block results are 
	 * propagated (or not) and whether or not subscriptions are in effect.
	 * 
	 * @param s the new state
	 */
	public abstract void setState(DiagramState s);
	
	/**
	 * Loop through all blocks and start them.
	 * By default there is no special ordering.
	 */
	public void startBlocks() {
		for( CoreBlock pb:getDiagramBlocks()) {
			pb.start();
		}
	}
	
	// This should only be called on a new diagram, or on new blocks for an existing
	// diagram. Note that "starting" a subscription on an existing property *should* 
	// do nothing.
	public void startSubscriptions() {
		//log.infof("%s.startSubscriptions: ...%d:%s",TAG,projectId,getName());
		for( CoreBlock pb:getDiagramBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				controller.startSubscription(pb,bp);
			}
		}
	}
	protected void stopSubscriptions() {
		log.infof("%s.stopSubscriptions: ...%d:%s",TAG,projectId,getName());
		for( CoreBlock pb:getDiagramBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				controller.removeSubscription(pb,bp);
			}
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
	public abstract void updateBlockTimers() ;
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
				CoreBlock upstreamBlock = blocks.get(pc.getSource());
				if( upstreamBlock!=null ) {
					BlockPort key = new BlockPort(upstreamBlock,pc.getUpstreamPortName());
					List<ProcessConnection> connections = outgoingConnections.get(key);
					if( connections==null ) {
						connections = new ArrayList<ProcessConnection>();
						outgoingConnections.put(key, connections);
						log.tracef("%s.analyze: mapping connection from %s:%s",TAG,upstreamBlock.getBlockId().toString(),pc.getUpstreamPortName());
					}
					if( !connections.contains(pc) ) connections.add(pc);
				}
				else {
					log.warnf("%s.analyze: Source block (%s) not found for connection",TAG,pc.getSource().toString());
				}
				CoreBlock downstreamBlock = blocks.get(pc.getTarget());
				if( downstreamBlock!=null ) {
					BlockPort key = new BlockPort(downstreamBlock,pc.getDownstreamPortName());
					List<ProcessConnection> connections = incomingConnections.get(key);
					if( connections==null ) {
						connections = new ArrayList<ProcessConnection>();
						incomingConnections.put(key, connections);
					}
					if( !connections.contains(pc) ) connections.add(pc);
				}
				else {
					log.warnf("%s.analyze: Target block (%s) not found for connection",TAG,pc.getTarget().toString());
				}
			}
			else {
				log.warnf("%s.analyze: %s has invalid serialized connection (%s)",TAG,diagram.getName(),invalidConnectionReason(sc));
			}

		}
	}
	/**
	 * Compare blocks in the serializable version to the current. Update and adjust subscriptions where necessary.
	 * 
	 * @param diagram the serializable template of this object.
	 */
	public abstract void updateProperties(SerializableDiagram diagram);
	
	/**
	 * Make sure all components of a serializable connection are present. 
	 * If not, the connection will be rejected.
	 * @param sc
	 * @return validity flag
	 */
	protected boolean validConnection(SerializableConnection sc) {
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
	 * Class for keyed storage of downstream block,port for a connection.
	 */
	public class BlockPort {
		private final String port;
		private final CoreBlock block;
		public BlockPort(CoreBlock block,String port) {
			this.port = port;
			this.block = block;
		}
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
		public CoreBlock getBlock() { return this.block; }
		
		public String getPort() { return this.port; }
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
		public String getSourceBlock() { return this.sourceBlock; }
		public String getSourcePort()  { return this.sourcePort; }
		public String getTargetBlock() { return this.targetBlock; }
		
		public String getTargetPort()  { return this.targetPort; }
		@Override
		public int hashCode() {
			return this.sourceBlock.hashCode()+this.sourcePort.hashCode()-this.targetBlock.hashCode()-this.targetPort.hashCode();
		}
	}
}
