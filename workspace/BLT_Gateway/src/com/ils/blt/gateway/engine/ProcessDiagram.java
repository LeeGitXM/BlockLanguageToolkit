/**
 *   (c) 2014-2022  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.AttributeDisplay;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.connection.ProcessConnection;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.ControllerRequestHandler;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;

/**
 * This diagram is the "model" that encapsulates the structure of the blocks and connections
 * of a ProcessDiagramView as viewed in the Designer.
 *  
 * This class provides answers to questions that the model control may ask about "what's next?".  
 * 
 *  The document is constant for the life of this instance.
 */
public class ProcessDiagram extends ProcessNode implements DiagnosticDiagram {
	private static final long serialVersionUID = 3557397875746466629L;
	private static String CLSS = "ProcessDiagram";
	private static final boolean DEBUG = false;
	private boolean valid = false;
	protected final Map<UUID,ProcessBlock> blocks;
	private final Map<ConnectionKey,ProcessConnection> connectionMap;            // Key by connection number
	protected final Map<String,AttributeDisplay> displays;						// Key is a property notification key (UUID,property name)
	protected final Map<BlockPort,List<ProcessConnection>> incomingConnections; // Key by downstream block:port
	protected final Map<BlockPort,List<ProcessConnection>> outgoingConnections;  // Key by upstream block:port
	private DiagramState state = DiagramState.UNSET;                             // So that new state will be a change
	private final BlockExecutionController controller = BlockExecutionController.getInstance();

	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the serializable version of this object.
	 * @param parent 
	 */
	public ProcessDiagram(SerializableDiagram diagm,ResourcePath parent,String projectName) { 
		super(diagm.getName(),parent,projectName,BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId());
		this.state = diagm.getState();
		blocks = new HashMap<UUID,ProcessBlock>();
		connectionMap = new HashMap<ConnectionKey,ProcessConnection>();
		displays = new HashMap<>();
		incomingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
		outgoingConnections = new HashMap<BlockPort,List<ProcessConnection>>();
	}

	public AttributeDisplay getAttributeDisplay(UUID id,String pname) { 
		String key = NotificationKey.keyForProperty(id.toString(), pname);
		return displays.get(key); 
	}

	public ProcessBlock getProcessBlock(UUID id) { return blocks.get(id); }
	@Override
	public ProcessBlock getProcessBlock(String idString) { 
		UUID id = UUID.fromString(idString);
		return blocks.get(id); 
	}
	
	// For now we just do a linear search
	public ProcessBlock getBlockByName(String name) { 
		ProcessBlock result = null;
		if( name!=null ) {
			for(ProcessBlock blk:getProcessBlocks()) {
				if(blk.getName().equals(name)) {
					result = blk;
					break;
				}
			}
		}
		return result;
	}

	public Collection<AttributeDisplay> getAttributeDisplays()    { return displays.values(); }
	public Collection<ProcessBlock> getProcessBlocks()            { return blocks.values(); }

	public String getProviderForState(DiagramState s) {
		String provider = "";
		if( s.equals(DiagramState.ISOLATED)) {
			provider = requestHandler.getProjectToolkitProperty(getProjectName(),ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER);
		}
		else {
			provider = requestHandler.getProjectToolkitProperty(getProjectName(),ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER);	
		}
		return provider;
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
	 * Remove blocks in this diagram that are NOT in the 
	 * supplied list. Kill any tag subscriptions associated with those blocks.
	 * This is called on a diagram save.
	 * @return a list of deleted blocks
	 */
	public List<ProcessBlock> removeUnusedBlocks(SerializableBlock[] newBlocks) {
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
			if( DEBUG ) log.infof("%s.removeBlocksFromList: decommissioned %s (%d)",CLSS,oldBlock.getName(),oldBlock.hashCode());
		}
		return blocksToRemove;
	}
	/**
	 * Clone attribute displays from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any blocks that currently
	 * exist. Newly created blocks are started.
	 * 
	 * Blocks with an updated version are updated and replaced in the serializablediagram
	 * A save is required after they are replaced.
	 * 
	 * @param displays, an array of newly created attribute displays to be added to the diagram.
	 */
	public void createAttributeDisplays(AttributeDisplay[] darray ) {
		for( AttributeDisplay display:darray ) {
			AttributeDisplay ad = display.clone();
			displays.put(ad.getBlockId().toString(),ad);
			ad.start();
		}
	}
	/**
	 * Clone blocks from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any blocks that currently
	 * exist. Newly created blocks are started.
	 * 
	 * Blocks with an updated version are updated and replaced in the serializablediagram
	 *    A save is required after they are replaced.
	 * 
	 * @param sblks an array of newly created blocks to be added to the diagram
	 */
	public void createBlocks(SerializableBlock[] sblks ) {
		BlockFactory blockFactory = BlockFactory.getInstance();
		String provider = getProviderForState(state);
		if( DEBUG ) log.infof("%s.createBlocks: Starting...", CLSS);
		// Update the blocks - we've already deleted any not present in the new
		for( SerializableBlock sb:sblks ) {
			UUID id = sb.getId();
			ProcessBlock pb = blocks.get(id);
			if( pb==null ) {
				pb = blockFactory.blockFromSerializable(getResourceId(),sb,getProjectName());
				if( pb!=null ) {
					// Set the proper timer
					if(DiagramState.ACTIVE.equals(state)) pb.setTimer(controller.getTimer());
					else if(DiagramState.ISOLATED.equals(state)) pb.setTimer(controller.getSecondaryTimer());
					pb.setProjectName(resourceId.getProjectName());
					blocks.put(pb.getBlockId(), pb);

					if( DEBUG ) log.infof("%s.createBlocks: New block %s(%d)", CLSS, pb.getName(), pb.hashCode());

					if( BlockExecutionController.CONTROLLER_RUNNING_STATE.equalsIgnoreCase(BlockExecutionController.getExecutionState()) ) {
						updatePropertyProviderInBlock(provider,pb);
						pb.start();
						// After we start the new block, then start any appropriate subscriptions
						for(BlockProperty bp:pb.getProperties()) {
							controller.startSubscription(getState(),pb,bp);
							// Source/Sink tag paths can be changed outside the property editor.
							if(  bp.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
								(pb.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE) ||
								 pb.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ) ) {
								controller.sendPropertyBindingNotification(pb.getBlockId().toString(), bp.getName(), bp.getBinding());
							}
						}
					}
				}
				else {
					log.errorf("%s.createBlocks: ERROR, diagram %s failed to instantiate block of type %s",CLSS,getName(),sb.getClassName());
				}
			}
		}
		if( DEBUG ) log.infof("%s.createBlocks: Complete!", CLSS);
		return;
	}
	/**
	 * Clone connections from the subject serializable diagram and add them to the current.
	 * In order to make this applicable for updates, we skip any connections that currently
	 * exist.
	 * 
	 * @param scxns an array of connection to be added
	 */
	public void updateConnections(SerializableConnection[] scxns) {
		ConnectionFactory connectionFactory = ConnectionFactory.getInstance();
		// Update the connections
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
						if( DEBUG ) log.infof("%s.updateConnections: mapping connection from %s:%s",CLSS,upstreamBlock.getBlockId().toString(),pc.getUpstreamPortName());
					}
					// This may be a new connection, so push its value.
					if( !connections.contains(pc) ) {
						connections.add(pc);
						controller.sendConnectionNotification(upstreamBlock.getBlockId().toString(), key.getPort(), upstreamBlock.getLastValue());
					}
				}
				else {
					log.warnf("%s.updateConnections: Source block (%s) not found for connection",CLSS,pc.getSource().toString());
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
					log.warnf("%s.updateConnections: Target block (%s) not found for connection",CLSS,pc.getTarget().toString());
				}
			}
			else {
				log.warnf("%s.updateConnections: %s has invalid serialized connection (%s)",CLSS,getName(),invalidConnectionReason(sc));
			}
		}
		// Now that the connection maps have been created/updated, allow the blocks to update their information.
		// Currently we only do this for the incoming connections.
		if( DEBUG ) log.infof("%s.updateConnections: updating connections ...",CLSS);
		for(BlockPort key:incomingConnections.keySet() ) {
			key.getBlock().validateConnections();
			
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
			if( pb!=null && sb.getProperties()!=null ) {
				if( DEBUG ) log.infof("%s.updateProperties: Update block %s",CLSS,pb.getName());
				boolean hasChanged = false;
				// Stop old subscriptions ONLY if the property changed, or no longer exists
				// NOTE: The blockFactory update will take care of values. We're just worried about subscriptions
				for( BlockProperty newProp:sb.getProperties() ) {
					if( newProp==null ) continue;  // Is this even possible?
					if( newProp.getName()==null ) {
						log.warnf("%s.updateProperties: Found a no-name property updating block %s:%s",CLSS,pb.getClassName(),pb.getName());
						continue;
					}
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
								controller.startSubscription(getState(),pb,prop);
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
							if( !prop.getBindingType().equals(BindingType.TAG_WRITE) ) {
								controller.removeSubscription(pb, prop);
								controller.startSubscription(getState(),pb,newProp);
							}
				
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
				log.errorf("%s.updateProperties: ERROR, block %s missing in diagram %s ",CLSS,sb.getName(),diagram.getName());
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
	 * @param incoming notification to broadcast
	 * @return a new value notification for the receiving block(s)
	 */
	public Collection<SignalNotification> getBroadcastNotifications(BroadcastNotification incoming) {
		Collection<SignalNotification>notifications = new ArrayList<SignalNotification>();
		if( incoming.getScope().equals(TransmissionScope.BLOCK)) {
			// Send to a single, specified block
			ProcessBlock block = getBlockByName(incoming.getBlockName());
			if( block!=null ) {
				SignalNotification sn = new SignalNotification(block,incoming.getValue());
				notifications.add(sn);
			}
			else {
				log.warnf("%s.getBroadcastNotifications: Target block %s not found in %s",CLSS,incoming.getBlockName(),getName());
			}
		}
		else {
			// This is really TransmissionScope.LOCAL
			for( ProcessBlock block:getProcessBlocks()) {
				if( !block.isReceiver() ) continue;
				SignalNotification sn = new SignalNotification(block,incoming.getValue());
				notifications.add(sn);
			}
		}
		
		return notifications;
	}
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the output(s) of the specified block.
	 */
	public List<ProcessBlock> getConnectedBlocksAtPort(ProcessBlock root,String port) {
		List<ProcessBlock> connectedBlocks = new ArrayList<>();
		BlockPort key = new BlockPort(root,port);
		List<ProcessConnection> cxns = incomingConnections.get(key);
		if( cxns!=null ) {
			for(ProcessConnection cxn:cxns) {
				UUID blockId = cxn.getSource();
				connectedBlocks.add(blocks.get(blockId));		
			}
		}
		else {
			cxns = outgoingConnections.get(key);
			if( cxns!=null ) {
				for(ProcessConnection cxn:cxns) {
					UUID blockId = cxn.getTarget();
					connectedBlocks.add(blocks.get(blockId));		
				}
			}
		}
		return connectedBlocks;
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
	 * Loop detection is pretty primitive. We stop if we ever hit the start block,
	 * but it is possible to have undetected sub-loops.
	 * 
	 * @param root the subject block
	 * @return a list of blocks connected to the input(s) of the specified block.
	 *         In the special case where the block is a source block, include
	 *         blocks connected to its associated sinks.
	 */
	public List<ProcessBlock> getUpstreamBlocksCrossingConnections(ProcessBlock root) {
		List<ProcessBlock> upstream = new ArrayList<>();
		UtilityFunctions fcns = new UtilityFunctions();
		for( AnchorPrototype ap:root.getAnchors() ) {
			if( ap.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
				String port = ap.getName();
				BlockPort key = new BlockPort(root,port);
				List<ProcessConnection> cxns = incomingConnections.get(key);
				if( cxns!=null ) {
					for(ProcessConnection cxn:cxns) {
						UUID blockId = cxn.getSource();
						if( blockId.equals(root.getBlockId())) break;  // A loop
						ProcessBlock blk = blocks.get(blockId);
						if( blk.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE)) {
							BlockProperty tagProperty = blk.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
							if( tagProperty!=null ) {
								String tagPath = fcns.providerlessPath(tagProperty.getBinding());
								if( tagPath!=null && tagPath.length()>0 ) {
									List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
									for(SerializableResourceDescriptor desc:descriptors) {
										ProcessDiagram diagram = controller.getDiagram(desc.getResourceId());
										for(ProcessBlock sink:diagram.getProcessBlocks()) {
											if( sink.getBlockId().equals(root.getBlockId())) continue;  // Skip the root
											if( sink.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK) ) {
												BlockProperty prop = sink.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
												if( prop!=null && tagPath.equalsIgnoreCase(fcns.providerlessPath(prop.getBinding()))  ) {
													List<ProcessBlock> sinkPredecessors = diagram.getUpstreamBlocks(sink);
													for(ProcessBlock sinkPredecessor:sinkPredecessors) {
														if( sinkPredecessor.getBlockId().equals(root.getBlockId())) continue;  // Skip the root
														upstream.add(sinkPredecessor);
													}
												}
											}
										}
									}
								}
							}
						}
						else if( !blk.getBlockId().equals(root.getBlockId())) {
							upstream.add(blk);
						}
					}
				}		
			}
		}
		return upstream;
	}
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the input(s) of the specified block.
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
	 * The controller is processing notifications of block output values. Determine which blocks are connected downstream,
	 * and create notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no downstream connection.
	 * @param  incoming new value notification of change
	 * @return a new value notification for the downstream block(s)
	 */
	public Collection<IncomingNotification> getOutgoingNotifications(OutgoingNotification incoming) {
		ProcessBlock block = incoming.getBlock();
		String port = incoming.getPort();
		QualifiedValue value = incoming.getValue();

		Collection<IncomingNotification>notifications = new ArrayList<IncomingNotification>();
		BlockPort key = new BlockPort(block,port);
		if( outgoingConnections.get(key)!=null ) {
			// Avoid concurrent modification exceptions by creating new collection
			List<ProcessConnection> cxns = new ArrayList<>(outgoingConnections.get(key));
			for(ProcessConnection cxn:cxns) {
				UUID blockId = cxn.getTarget();
				ProcessBlock blk = blocks.get(blockId);
				if( blk!=null ) {
					IncomingNotification vcn = new IncomingNotification(cxn,value);
					notifications.add(vcn);
				}
				else {
					log.warnf("%s.getOutgoingNotifications: Target block %s not found for connection",CLSS,blockId.toString());
				}
			}

		}
		else {
			if( DEBUG ) log.infof("%s.getOutgoingNotifications: no connections found for %s:%s",CLSS,block.getBlockId().toString(),port);

		}
		return notifications;
	}
	
	/**
	 * The controller is processing notifications of a block output signal. Determine which blocks are connected downstream,
	 * and create signal notifications for each. In/Out are from the point of view of a block, so are backwards here.
	 * An empty return indicates no downstream connection.
	 * @param incoming new signal notification of an incoming change
	 * @return a collection of  new signal notification for the downstream block(s)
	 */
	public Collection<SignalNotification> getOutgoingSignalNotifications(OutgoingNotification incoming) {
		ProcessBlock block = incoming.getBlock();
		String port = incoming.getPort();
		QualifiedValue value = incoming.getValue();

		Collection<SignalNotification>notifications = new ArrayList<SignalNotification>();
		BlockPort key = new BlockPort(block,port);
		if( outgoingConnections.get(key)!=null ) {
			List<ProcessConnection> cxns = new ArrayList<>(outgoingConnections.get(key));
			for(ProcessConnection cxn:cxns) {
				UUID blockId = cxn.getTarget();
				ProcessBlock blk = blocks.get(blockId);
				if( blk!=null ) {
					SignalNotification vcn = new SignalNotification(blk,value);
					notifications.add(vcn);
				}
				else {
					log.warnf("%s.getOutgoingSignalNotifications: Target block %s not found for connection",CLSS,blockId.toString());
				}
			}
		}
		else {
			if( DEBUG ) log.infof("%s.getOutgoingSignalNotifications: no connections found for %s:%s",CLSS,block.getBlockId().toString(),port);
		}
		return notifications;
	}
	
	public DiagramState getState() {return state;}
	public void setState(DiagramState s) {
		setState(s,false);
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
	 * @param force if true execute even if there is no state change. This stops and starts the tag subscriptions.
	 */
	public void setState(DiagramState s,boolean force) {
		if( DEBUG ) log.infof("%s.setState: %s->%s", CLSS,getState().name(),s.name());
		// Only restart subscriptions on a change.
		if( force || !s.equals(getState())) {
			// Check if we need to stop current subscriptions
			if( !DiagramState.DISABLED.equals(getState()) ) {
				stopSubscriptions();
			}
			// Stop blocks
			for(ProcessBlock blk:blocks.values()) {
				blk.stop();
			}
			
			// Set new state
			this.state = s;
			
			// Set correct timer for new state
			updateBlockTimers(s);

			// If the new state is active or isolated, start subscriptions
			if(!DiagramState.DISABLED.equals(state) ) {
				String provider = getProviderForState(state);	
				updatePropertyProviders(provider);                    // Does not start subscriptions
				
				// The two-phase start is probably not necessary here
				// since we start the subscriptions after starting the blocks,
				// but we'll do it anyway for consistency
				for(ProcessBlock blk:blocks.values()) {
					if( !blk.delayBlockStart() ) blk.start();
				}
				

				
				// The Inputs should not propagate an old value, 
				// but instead react to the new subscriptions
				for(ProcessBlock blk:blocks.values()) {
					if( blk.delayBlockStart() ) {
						blk.start();
					}
				}
				
				//Restart subscriptions for the new state
				startSubscriptions(state);
	
			}

			// Fire diagram notification change
			controller.sendStateNotification(resourceId, s.name());
		}
	}
	/**
	 * Report on whether or not the DOM contained more than one connected node.
	 */
	public boolean isValid() { return valid; }
	@Override
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = super.toResourceDescriptor();
		descriptor.setIsFolder(false);
		return descriptor;
	}
	/**
	 * Loop through the diagram's blocks setting the timer appropriate
	 * to the diagram's state.
	 */
	public void updateBlockTimers(DiagramState s) {
		// Set the proper timer
		WatchdogTimer timer = controller.getTimer();
		if( DiagramState.ISOLATED.equals(s) )  timer = controller.getSecondaryTimer();
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
	// diagram or an a change of state for an existing diagram. Note that "starting" 
	// a subscription on an existing property *should* do nothing.
	public void startSubscriptions(DiagramState s) {
		//log.infof("%s.startSubscriptions: ...%d:%s",TAG,projectId,getName());
		for( ProcessBlock pb:getProcessBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				if(DEBUG && bp.getBinding()!=null && !bp.getBinding().isEmpty()) {
					log.infof("%s.startSubscriptions: %s.%s %s",CLSS,pb.getName(),bp.getName(),bp.getBinding());
				}
				controller.startSubscription(s,pb,bp);
			}
		}
	}
	
	/**
	 * Stop all subscriptions for properties in blocks in this diagram
	 */
	public void stopSubscriptions() {
		if( DEBUG ) log.infof("%s.stopSubscriptions: project %s:%s",CLSS,resourceId.getProjectName(),getName());
		for( ProcessBlock pb:getProcessBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				if(DEBUG && bp.getBinding()!=null && !bp.getBinding().isEmpty()) {
					log.infof("%s.stopSubscriptions: %s.%s %s",CLSS,pb.getName(),bp.getName(),bp.getBinding());
				}
				controller.removeSubscription(pb,bp);
			}
		}
	}

	/**
	 * For every property that is bound to a tag, update its provider. 
	 * NOTE: This does NOT change subscriptions.
	 * @param provider current tag provider
	 */
	public void updatePropertyProviders(String provider) {
		for( ProcessBlock pb:getProcessBlocks()) {
			updatePropertyProviderInBlock(provider,pb);
		}
	}
	public void updatePropertyProviderInBlock(String provider,ProcessBlock pb) {
		for(BlockProperty bp:pb.getProperties()) {
			BindingType bindingType = bp.getBindingType();
			if( bindingType.equals(BindingType.TAG_MONITOR) ||
					bindingType.equals(BindingType.TAG_READ)    ||
					bindingType.equals(BindingType.TAG_WRITE)   ||
					bindingType.equals(BindingType.TAG_READWRITE) ) {

				String tagPath = bp.getBinding();
				String newPath = replaceProviderInPath(tagPath,provider);
				if( !tagPath.equals(newPath)) {
					bp.setBinding(newPath);
					controller.sendPropertyBindingNotification(pb.getBlockId().toString(), bp.getName(), newPath);
				}
			}	
		}
	}
	
	public String replaceProviderInPath(String path,String providerName) {
		if( !path.isEmpty() ) {
			int pos = path.indexOf("]");
			if( pos>0 ) path = path.substring(pos+1);
			if( !path.isEmpty() ) path =  String.format("[%s]%s", providerName,path);
		}
		return path;
	}
	
	/**
	 * For every property that is bound to a tag, make sure that it is subscribed to the proper provider. 
	 */
	public void synchronizeSubscriptions() {
		if(DiagramState.DISABLED.equals(getState())) return;
		String provider = null;
		if( DiagramState.ISOLATED.equals(getState())) {
			provider = ControllerRequestHandler.getInstance().getProjectToolkitProperty(getProjectName(),ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER);
		}
		else {
			provider = ControllerRequestHandler.getInstance().getProjectToolkitProperty(getProjectName(),ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER);
		}
		
		for( ProcessBlock pb:getProcessBlocks()) {
			for(BlockProperty bp:pb.getProperties()) {
				BindingType bindingType = bp.getBindingType();
				if( bindingType.equals(BindingType.TAG_MONITOR) ||
					bindingType.equals(BindingType.TAG_READ)    ||
					bindingType.equals(BindingType.TAG_READWRITE) ) {
					
					String tagPath = bp.getBinding();
					if( tagPath==null || tagPath.isEmpty()) continue;
					String properPath = replaceProviderInPath(tagPath,provider);
					String path = controller.getSubscribedPath(pb, bp);
					if( !properPath.equals(path)) {
						log.infof("%s.validateSubscriptions: Auto-correcting binding for %s:%s (%s to %s)",CLSS,pb.getName(),
											bp.getName(),path,properPath);
						controller.removeSubscription(pb, bp);
						bp.setBinding(properPath);
						controller.startSubscription(getState(),pb, bp);
						controller.sendPropertyBindingNotification(pb.getBlockId().toString(), bp.getName(), properPath);
					}
				}	
			}
		}
	}
	
	private UUID makeUUID(String name) {
		UUID uuid = null;
		try {
			uuid = UUID.fromString(name);
		}
		catch(IllegalArgumentException iae) {
			uuid = UUID.nameUUIDFromBytes(name.getBytes());
		}
		return uuid;
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
