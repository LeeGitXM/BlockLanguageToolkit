/**
 *   (c) 2012-2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.control.IncomingNotification;
import com.ils.block.control.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.connection.Connection;
import com.ils.connection.ProcessConnection;
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
public class ProcessDiagram {
	
	private static String TAG = "ProcessDiagram";
	private final LoggerEx log;
	private final SerializableDiagram diagram;
	private boolean valid = false;
	private final long projectId;
	private final long resourceId;
	private final Map<UUID,ProcessBlock> blocks;
	private final Map<ConnectionKey,ProcessConnection> connections;              // Key by connection number
	private final Map<BlockPort,List<ProcessConnection>> outgoingConnections;    // Key by upstream block:port
	
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param dom the unserialized object that represents the diagram. 
	 */
	public ProcessDiagram(SerializableDiagram dom,long proj,long res) { 
		this.diagram = dom;
		this.projectId = proj;
		this.resourceId = res;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		blocks = new HashMap<UUID,ProcessBlock>();
		connections = new HashMap<ConnectionKey,ProcessConnection>();
		outgoingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
		analyze(diagram);
	}
	
	public ProcessBlock getBlock(UUID id) { return blocks.get(id); }
	
	public Collection<ProcessBlock> getProcessBlocks() { return blocks.values(); }
	
	/**
	 * Analyze the diagram for nodes.
	 */
	private void analyze(SerializableDiagram diagram) {
		log.debugf("%s.analyze: %s ....%d:%d",TAG,diagram.getName(),projectId,resourceId);
		
		BlockFactory blockFactory = BlockFactory.getInstance();
		ConnectionFactory connectionFactory = ConnectionFactory.getInstance();
		
		// Update the blocks
		SerializableBlock[] sblks = diagram.getBlocks();
		for( SerializableBlock sb:sblks ) {
			UUID id = sb.getId();
			ProcessBlock pb = blocks.get(id);
			if( pb==null ) {
				pb = blockFactory.blockFromSerializable(projectId,resourceId,sb);
				if( pb!=null ) blocks.put(pb.getBlockId(), pb);
				else log.errorf("%s.analyze: ERROR %s failed to instantiate %s",TAG,diagram.getName(),sb.getClassName());
			}
			else {
				blockFactory.updateBlockFromSerializable(pb,sb);
			}
		}
		// Update the connections
		SerializableConnection[] scxns = diagram.getConnections();
		for( SerializableConnection sc:scxns ) {

			ConnectionKey cxnkey = new ConnectionKey(sc.getBeginBlock().toString(),sc.getBeginAnchor().getId().toString(),
					                             sc.getEndBlock().toString(),sc.getEndAnchor().getId().toString());
			ProcessConnection pc = connections.get(cxnkey);
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
				connections.add(pc);
			}
			else {
				log.warnf("%s.analyze: Source block (%s) not found for connection",TAG,pc.getSource().toString());
			}
		}
		log.debugf("%s.analyze: Complete .... %d blocks and %d connections",TAG,diagram.getBlocks().length,diagram.getConnections().length);
	}

	/**
	 * @return a Connection from the diagram given its id.
	 */
	public Connection getConnection(String id) { return connections.get(id); }
	
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
		Object value = incoming.getValue();
		
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
			log.warnf("%s.getOutgoingNotifications: no connections found for %s:%s",TAG,block.getBlockId().toString(),port);
		}
		return notifications;
	}
	
	/**
	 * Report on whether or not the DOM contained more than one connected node.
	 */
	public boolean isValid() { return valid; }
	
	/**
	 * Class for keyed storage of downstream block,port for a connection.
	 */
	private class BlockPort {
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
	private class ConnectionKey {
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
