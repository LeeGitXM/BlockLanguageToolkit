/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.connection.ProcessConnection;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This diagram is the "model" that encapsulates the structure of the blocks and connections
 * of a ProcessDiagramView as viewed in the Designer.
 *  
 * This class provides answers to questions that the model control may ask about "what's next?".  
 * 
 *  The document is constant for the life of this instance.
 */
public class ProcessDiagram extends ProcessNode {
	
	private static String TAG = "ProcessDiagram";
	private final LoggerEx log;
	private final SerializableDiagram diagram;
	private boolean valid = false;
	protected final Map<UUID,ProcessBlock> blocks;
	private final Map<ConnectionKey,ProcessConnection> connectionMap;            // Key by connection number
	protected final Map<BlockPort,List<ProcessConnection>> outgoingConnections;   // Key by upstream block:port
	private long projectId = -1;
	private final long resourceId;
	private DiagramState state = DiagramState.ACTIVE;
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the unserialized object that represents the diagram.
	 * @param parent 
	 */
	public ProcessDiagram(SerializableDiagram diagm,UUID parent) { 
		super(diagm.getName(),parent,diagm.getId());
		this.diagram = diagm;
		this.state = diagm.getState();
		this.resourceId = diagm.getResourceId();
		log = LogUtil.getLogger(getClass().getPackage().getName());
		blocks = new HashMap<UUID,ProcessBlock>();
		connectionMap = new HashMap<ConnectionKey,ProcessConnection>();
		outgoingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
		analyze(diagram);
	}

	public ProcessBlock getBlock(UUID id) { return blocks.get(id); }
	public Collection<ProcessBlock> getProcessBlocks() { return blocks.values(); }
	public long getResourceId() { return this.resourceId; }
	public long getProjectId() {return projectId;}
	public void setProjectId(long projectId) {this.projectId = projectId;}
	
	/**
	 * Prepare for an update from a newly deserialized node.
	 */
	public void clearConnections() {
		connectionMap.clear();
		outgoingConnections.clear();
	}
	/**
	 * Remove blocks in this diagram that are NOT in the 
	 * supplied list.
	 */
	public void removeBlocksFromList(SerializableBlock[] newBlocks) {
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
	 * Analyze the diagram for nodes. This is valid as long as the existing diagram
	 * has no blocks that are NOT represented in the serializable version.
	 * 
	 * During this process we stop any existing blocks and remove tag subscriptions.
	 * The ModelManager restarts the blocks once everything is in place.
	 */
	public void analyze(SerializableDiagram diagram) {
		log.infof("%s.analyze: %s ...",TAG,diagram.getName());
		
		BlockFactory blockFactory = BlockFactory.getInstance();
		ConnectionFactory connectionFactory = ConnectionFactory.getInstance();
		
		// Update the blocks
		SerializableBlock[] sblks = diagram.getBlocks();
		for( SerializableBlock sb:sblks ) {
			UUID id = sb.getId();
			ProcessBlock pb = blocks.get(id);
			if( pb==null ) {
				pb = blockFactory.blockFromSerializable(getSelf(),sb);
				if( pb!=null ) {
					blocks.put(pb.getBlockId(), pb);
					log.infof("%s.analyze: New block %s(%d)",TAG,pb.getName(),pb.hashCode());
				}
				else log.errorf("%s.analyze: ERROR %s failed to instantiate %s",TAG,diagram.getName(),sb.getClassName());
			}
			else {
				log.infof("%s.analyze: Update block %s(%d)",TAG,pb.getName(),pb.hashCode());
				pb.stop();
				// Stop old subscriptions
				for(BlockProperty prop:pb.getProperties()) {
					controller.removeSubscription(pb, prop);
				}
				blockFactory.updateBlockFromSerializable(pb,sb);
			}
		}
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
						log.tracef("%s.analyze: mapping connection from %s:%s",TAG,upstreamBlock.getBlockId().toString(),pc.getUpstreamPortName());
					}
					if( !connections.contains(pc) ) connections.add(pc);
				}
				else {
					log.warnf("%s.analyze: Source block (%s) not found for connection",TAG,pc.getSource().toString());
				}
			}
			else {
				log.warnf("%s.analyze: %s has invalid serialized connection (%s)",TAG,diagram.getName(),invalidConnectionReason(sc));
			}

		}
		log.infof("%s.analyze: Complete .... %d blocks and %d connections",TAG,diagram.getBlocks().length,diagram.getConnections().length);
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
		List<ProcessConnection> cxns = outgoingConnections.get(key);
		if( cxns!=null ) {
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
	 * propagated (or not).
	 * 
	 * @param s the new state
	 */
	public void setState(DiagramState s) {this.state = s;}
	/**
	 * Report on whether or not the DOM contained more than one connected node.
	 */
	public boolean isValid() { return valid; }
	
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
