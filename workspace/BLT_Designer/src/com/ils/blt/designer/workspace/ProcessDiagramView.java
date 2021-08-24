/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.swing.JOptionPane;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableAnchorPoint;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.NotificationHandler;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualityCode;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.sqltags.model.types.ExpressionType;
import com.inductiveautomation.ignition.common.util.AbstractChangeable;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.LookupConnection;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This class represents a diagram in the designer.
 */
public class ProcessDiagramView extends AbstractChangeable implements BlockDiagramModel,NotificationChangeListener {
	private static ILSLogger log = LogMaker.getLogger(ProcessDiagramView.class.getPackage().getName());
	// Use TAG as the "source" identifier when registering for notifications from Gateway
	private static final String CLSS = "ProcessDiagramView";
	private final ApplicationRequestHandler appRequestHandler;
	private final Map<UUID,ProcessBlockView> blockMap = new HashMap<>();
	private List<Connection> connections = new ArrayList<>();
	private static final int MIN_WIDTH = 200;
	private static final int MIN_HEIGHT = 200;
	private static final boolean DEBUG = true;
	private Dimension diagramSize = new Dimension(MIN_WIDTH,MIN_HEIGHT);
	private final UUID id;
	private String name = "UNSET";
	private UUID encapsulationBlockID = null;  // Used only if this diagram represents a sub-workspace
	private final ProjectResourceId resourceId;
	private DiagramState state = DiagramState.ACTIVE;
	private DesignerContext context;
	private boolean dirty = false;   // A newly created diagram is "dirty" until it is saved
	                                 // but this looks better. It'll be dirty again with the first block
	private boolean suppressStateChangeNotification = false;
	private String watermark = "";
	
	/**
	 * Constructor: Create an instance given a SerializableDiagram. Do a save to synchronize this
	 *              with the gateway.
	 * @param resid
	 * @param diagram
	 */
	public ProcessDiagramView (ProjectResourceId resid,SerializableDiagram diagram, DesignerContext context) {
		this(resid,diagram.getId(),diagram.getName());
		if( DEBUG ) log.infof("%s.ProcessDiagramView: for diagram %s", CLSS, diagram.getName());
		this.state = diagram.getState();
		this.watermark = diagram.getWatermark();
		this.context = context;
		suppressStateChangeNotification = true;
		synchronized (diagram) {
			for( SerializableBlock sb:diagram.getBlocks()) {
				ProcessBlockView pbv = new ProcessBlockView(sb);
				blockMap.put(sb.getId(), pbv);
				if( DEBUG ) log.infof("%s.ProcessDiagramView: Added a ProcessBlockView for %s to map", CLSS, sb.getName());
				this.addBlock(pbv);
			}
			
			for( SerializableConnection scxn:diagram.getConnections() ) {
				SerializableAnchorPoint a = scxn.getBeginAnchor();
				SerializableAnchorPoint b = scxn.getEndAnchor();
				if( a!=null && b!=null ) {
					ProcessBlockView blocka = blockMap.get(a.getParentId());
					ProcessBlockView blockb = blockMap.get(b.getParentId());
					if( blocka!=null && blockb!=null) {
						if( DEBUG ) log.infof("%s.ProcessDiagramView: Creating a connection from %s to %s...", CLSS, blocka.getName(), blockb.getName());
						AnchorPoint origin = new ProcessAnchorView(blocka,a);
						AnchorPoint terminus = new ProcessAnchorView(blockb,b);
						this.addConnection(origin,terminus);   // AnchorPoints
						// Update most recent value from serialized connection anchor point
						QualifiedValue qv = new BasicQualifiedValue(a.getLastValue(),QualityCode.Good);
						blocka.recordLatestValue(origin.getId().toString(), qv);
					}
					else {
						if( blocka==null ) {
							log.warnf("%s.createDiagramView: Failed to find block %s for begin anchor point %s",CLSS,a.getParentId(),a);
						}
						if( blockb==null ) {
							log.warnf("%s.createDiagramView: Failed to find block %s for end anchor point %s",CLSS,b.getParentId(),b);
						}
					}
				}
				else {
					log.warnf("%s.createDiagramView: Connection %s missing one or more anchor points",CLSS,scxn.toString());
				}
			}
			suppressStateChangeNotification = false;
		}  // -- end synchronized
		
		// Do this at the end to override state change on adding blocks/connectors.
		// Note this shouldn't represent a change for parents
		this.dirty = diagram.isDirty();
		// Compute diagram size to include all blocks
		// We do this initially. From then on it's whatever the user leaves it at.
		double maxX = MIN_WIDTH;
		double maxY = MIN_HEIGHT;
		for(ProcessBlockView blk:blockMap.values()) {
			if( blk.getLocation().getX()+blk.getPreferredWidth()>maxX ) maxX = blk.getLocation().getX()+blk.getPreferredWidth();
			if( blk.getLocation().getY()+blk.getPreferredHeight()>maxY) maxY = blk.getLocation().getY()+blk.getPreferredHeight();
		}
		
		diagramSize =  new Dimension((int)(maxX*1.05),(int)(maxY*1.25));
		this.setDiagramSize(diagramSize);
	}
	
	/**
	 * Constructor with no context ...
	 * @param resId
	 * @param uuid
	 * @param nam
	 */
	public ProcessDiagramView(ProjectResourceId resId,UUID uuid, String nam) {
		this.id = uuid;
		this.resourceId = resId;
		this.name = nam;
		this.appRequestHandler = new ApplicationRequestHandler();
	}
	
	/** 
	 * Get the current block property values from the Gateway. 
	 * If the block does not have a Gateway counterpart (e.g. diagram is dirty), we'll get the 
	 * default property list for the block class. 
	 * 
	 * IMPORTANT: Always do this before block is displayed.
	 */
	public void initBlockProperties(ProcessBlockView block) {
		if( appRequestHandler!=null ) {
			List<BlockProperty> properties = appRequestHandler.getBlockProperties(block.getClassName(),
					resourceId,block.getId());
			for(BlockProperty bp:properties) {
				block.setProperty(bp);
			}
			log.tracef("%s.initBlockProperties - initialize property list for %s (%d properties)",CLSS,block.getId().toString(),properties.size());
		}
	}

	/**
	 * At the time that we add a new block, make sure that the block has a unique name.
	 * If it has no properties (e.g. was created from palette), then attempt to get 
	 * properties from its gateway counterpart. A newly imported block should already
	 * have properties.
	 * 
	 * @param blk block to be added
	 */
	@Override
	public void addBlock(Block blk) {
		if( blk instanceof ProcessBlockView) {
			ProcessBlockView block = (ProcessBlockView) blk;
			if( ((ProcessBlockView) blk).getProperties().isEmpty() ) initBlockProperties(block);
			log.tracef("%s.addBlock - %s",CLSS,block.getClassName());
			blockMap.put(blk.getId(), block);
			fireStateChanged();
		}
	}
	/**
	 * This is called from the super-class addConnection() method when a new connection
	 * is drawn. In this case, the anchor points don't necessarily have the most current  
	 * data type. We have to interrogate the current block.
	 */
	@Override
	public void addConnection(AnchorPoint begin, AnchorPoint end) {
		if( begin!=null && end!=null) {

			boolean disallow = false;
			BasicAnchorPoint eapp = null;
			// check if any input connections
			if( end instanceof BasicAnchorPoint && begin instanceof BasicAnchorPoint ) {
				eapp = (BasicAnchorPoint)end;
				ConnectionType originType = null;


				ProcessBlockView origin = (ProcessBlockView)begin.getBlock();
				BasicAnchorPoint bap = (BasicAnchorPoint)begin;
				for(ProcessAnchorDescriptor pad:origin.getAnchors()) {
					if( pad.getDisplay().equals(bap.getId())) {
						originType = pad.getConnectionType();
						break;
					}
				}

				// If only 1 input allowed, check to make sure it isn't already used and don't block if initializing
				if (!eapp.allowConnectionType(originType) && !suppressStateChangeNotification) { 
					disallow = true;
					String msg = String.format("Rejected connection.  Cannot connect %s and %s",eapp.getConnectionType().name(),originType.name());
					JOptionPane.showMessageDialog(null, msg, "Warning", JOptionPane.INFORMATION_MESSAGE);
					msg = String.format("%s.addConnection - rejected connection.  Cannot connect %s and %s",CLSS,eapp.getConnectionType().name(),originType.name());
					log.warnf(msg);
				}


				// check if input connection is of the correct type
				// only 1 input allowed, check to make sure it isn't already used and don't block if initializing
				if (!eapp.allowMultipleConnections() && !suppressStateChangeNotification) { 
					for(Connection cxn:connections) {
						if(cxn.getTerminus().equals(end)) {
							disallow = true;
							log.warnf("%s.addConnection - rejected attempt to add a second connection to a single connection endpoint",CLSS);
							break;
						}
					}
				}
			}

			if (!disallow) { 
				// Update the connection type from the current beginning anchor point
				// However, if the type is ANY or String, then alter the to match the end
				if( begin.getBlock() instanceof ProcessBlockView && begin instanceof BasicAnchorPoint ) {
					ProcessBlockView origin = (ProcessBlockView)begin.getBlock();
					BasicAnchorPoint bap = (BasicAnchorPoint)begin;
					for(ProcessAnchorDescriptor pad:origin.getAnchors()) {
						if( pad.getDisplay().equals(bap.getId())) {
							if(pad.getConnectionType().equals(ConnectionType.ANY) ||
									pad.getConnectionType().equals(ConnectionType.TEXT)  ) {
								bap.setConnectionType(eapp.getConnectionType());
								pad.setConnectionType(eapp.getConnectionType());
							}
							else {
								bap.setConnectionType(pad.getConnectionType());
							}
							break;
						}
					}
				}
				Connection cxn = new LookupConnection(this,begin,end);
				connections.add(cxn);
				fireStateChanged();
			}
		}

		else {
			log.warnf("%s.addConnection - rejected attempt to add a connection with null anchor",CLSS);
		}
	}

	// NOTE: This does not set connection type
	private SerializableConnection convertConnectionToSerializable(Connection cxn) {
		SerializableConnection result = new SerializableConnection();
		if( cxn.getOrigin()!=null && cxn.getTerminus()!=null ) {
			AnchorPoint ap = cxn.getOrigin();
			ProcessBlockView beginBlock = (ProcessBlockView)ap.getBlock();
			String port = ap.getId().toString();
			result.setBeginBlock(ap.getBlock().getId()); 
			result.setEndBlock(cxn.getTerminus().getBlock().getId());
			SerializableAnchorPoint sap = createSerializableAnchorPoint(cxn.getOrigin());
			QualifiedValue qv = beginBlock.getLastValueForPort(port);
			if( qv!=null ) {
				sap.setLastQuality(qv.getQuality().getName());
				sap.setLastValue(qv.getValue());
			}
			result.setBeginAnchor(sap);
			result.setEndAnchor(createSerializableAnchorPoint(cxn.getTerminus()));
		}
		else {
			log.warnf("%s.convertConnectionToSerializable: connection missing terminus or origin (%s)",CLSS,cxn.getClass().getName());
		}
		return result;
	}
	
	/**
	 * NOTE: This would normally be an alternative constructor for SerializableAnchorPoint.
	 *        Problem is that we need to keep that class free of references to Designer-only
	 *        classes (e.g. AnchorPoint).
	 * @param anchor
	 */
	private SerializableAnchorPoint createSerializableAnchorPoint(AnchorPoint anchor) {
		SerializableAnchorPoint sap = new SerializableAnchorPoint();
		if(anchor.isConnectorOrigin()) sap.setDirection(AnchorDirection.OUTGOING);
		else sap.setDirection(AnchorDirection.INCOMING);
		sap.setId(anchor.getId());
		sap.setParentId(anchor.getBlock().getId());
		sap.setAnchorX(anchor.getAnchor().x);
		sap.setAnchorY(anchor.getAnchor().y);
		sap.setHotSpot(anchor.getHotSpot().getBounds());
		sap.setPathLeaderX(anchor.getPathLeader().x);
		sap.setPathLeaderY(anchor.getPathLeader().y);
		return sap;
	}
	/**
	 * Create a POJO object from this model suitable for JSON serialization.
	 * @return an equivalent serializable diagram.
	 */
	public SerializableDiagram createSerializableRepresentation() {
		SerializableDiagram diagram = new SerializableDiagram();
		diagram.setName(name);
		diagram.setResourceId(resourceId);
		diagram.setId(getId());
		diagram.setState(state);
		diagram.setDirty(dirty);
		diagram.setWatermark(watermark);
		List<SerializableBlock> sblocks = new ArrayList<SerializableBlock>();
		for( ProcessBlockView blk:blockMap.values()) {
			SerializableBlock sb = blk.convertToSerializable();
			sblocks.add(sb);
		}
		diagram.setBlocks(sblocks.toArray(new SerializableBlock[sblocks.size()]));
			
		// As we iterate the connections, update SerializableAnchors with connection types
		List<SerializableConnection> scxns = new ArrayList<SerializableConnection>();
		for( Connection cxn:connections) {
			SerializableConnection scxn = convertConnectionToSerializable(cxn);
			// Set the connection type to the begin block type
			ProcessBlockView beginBlock = blockMap.get(scxn.getBeginBlock());
			if( beginBlock!=null ) {
				String port = scxn.getBeginAnchor().getId().toString();
				SerializableAnchorPoint sap = scxn.getBeginAnchor();
				boolean found = false;
				for(ProcessAnchorDescriptor desc:beginBlock.getAnchors()) {
					if( desc.getDisplay().equalsIgnoreCase(port) ) {
						found = true;
						scxn.setType(desc.getConnectionType());
						QualifiedValue qv = desc.getLastValue();
						if( qv!=null ) {
							sap.setLastQuality(qv.getQuality().getName());
							sap.setLastValue(qv.getValue());
						}
					}
				}
				if( !found ) log.warnf("%s.createSerializableRepresentation: unable to find %s port in begin block",CLSS,port);
			}
			else {
				log.warnf("%s.createSerializableRepresentation: begin block lookup failed",CLSS);
			}
			scxns.add(scxn);
		}
		diagram.setConnections(scxns.toArray(new SerializableConnection[scxns.size()]));
		return diagram;
	}
	
	@Override
	public void deleteBlock(Block blk) {
		// Delete every connection attached to the block
		List<Connection> connectionsToBeDeleted = new ArrayList<Connection>();
		UUID blockId = blk.getId();
		for( AnchorPoint ap:blk.getAnchorPoints()) {
			boolean isOrigin = ap.isConnectorOrigin();
			for(Connection cxn:connections) {
				if(isOrigin && blockId.equals(cxn.getOrigin().getBlock().getId()) )         connectionsToBeDeleted.add(cxn);
				else if(!isOrigin && blockId.equals(cxn.getTerminus().getBlock().getId()) ) connectionsToBeDeleted.add(cxn);
			}
		}
		
		for(Connection cxn:connectionsToBeDeleted) {
			connections.remove(cxn);
		}
		
		log.infof("%s.deleteBlock: deleting a sink (%s)",CLSS,blk.getClass().getCanonicalName());
		
		// For a Sink, remove its bound tag
		if( blk instanceof ProcessBlockView ) {
			ProcessBlockView view = (ProcessBlockView)blk;
			if( view.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)) {
				BlockProperty prop = view.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
				List<SerializableBlockStateDescriptor> sources = appRequestHandler.listSourcesForSink(getResourceId(),
						view.getId().toString());
				String tp = prop.getBinding();
				appRequestHandler.deleteTag(tp);
				if( sources.size()>0 ) {
					StringBuffer msg = new StringBuffer("The following SourceConnections are no longer connected:\n");
					for(SerializableBlockStateDescriptor source:sources) {
						msg.append("\t");
						msg.append(appRequestHandler.getDiagramForBlock(source.getIdString()).getName());
						msg.append(": ");
						msg.append(source.getName());
						msg.append("\n");
					}
					JOptionPane.showMessageDialog(null, msg.toString(), "Warning", JOptionPane.INFORMATION_MESSAGE);
				}
			}
		}
		// Delete the block by removing it from the map
		blockMap.remove(blk.getId());
		fireStateChanged();
	}
	
	@Override
	public void deleteConnection(AnchorPoint begin, AnchorPoint end) {
		boolean success = false;
		for(Connection cxn:connections) {
			if( cxn.getOrigin().equals(begin) && cxn.getTerminus().equals(end)) {
				connections.remove(cxn);
				success = true;
				fireStateChanged();
				break;
			}
		}
		if(!success) log.warnf("%s.deleteConnection: failed to find match to existing",CLSS);
	}
	public List<ProcessBlockView> getAttributeDisplays() {
		List<ProcessBlockView> list = new ArrayList<>();
		for(ProcessBlockView blk:blockMap.values()) {
			if( blk.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_ATTRIBUTE) ) {
				list.add(blk);
			}
		}
		return list;
	}
	/**
	 * @return a background color appropriate for the current state
	 *         of the diagram
	 */
	public Color getBackgroundColorForState() {
		Color result = BLTProperties.DIAGRAM_ACTIVE_BACKGROUND;
		if( getState().equals(DiagramState.ISOLATED)) result = BLTProperties.DIAGRAM_ISOLATED_BACKGROUND;
		else if( getState().equals(DiagramState.DISABLED)) result = BLTProperties.DIAGRAM_DISABLED_BACKGROUND;
		else if( isDirty() ) result = BLTProperties.DIAGRAM_DIRTY_BACKGROUND;
		return result;
	}
	
	@Override
	public Block getBlock(UUID key) {
		return blockMap.get(key);
	}
	@Override
	public Iterable<? extends Block> getBlocks() {
		return blockMap.values();
	}

	@Override
	public UUID getConnectedSetRoot() {
		return null;
	}

	@Override
	public Collection<Connection> getConnections() {
		return connections;
	}

	@Override
	public String getDiagramName() {return name;}
	public void setDiagramName(String nam) { this.name = nam; }

	@Override
	public Dimension getDiagramSize() {
		return diagramSize;
	}
	public UUID getEncapsulationBlockID() {return encapsulationBlockID;}
	public UUID getId() {return id;}

	public String getName() {return name;}

	// Required by the Block and Connector framework
	@Override
	public ResourcePath getResourcePath() {
		return resourceId.getResourcePath();
	}
	
	public ProjectResourceId getResourceId() {
		return resourceId;
	}
	
	public String getWatermark() {return watermark;}
	public void setWatermark(String mark) { this.watermark = mark; }

	@Override
	public boolean isConnectionValid(AnchorPoint startingAnchor, AnchorPoint endAnchor) {
		return ProcessBlockView.isConnectionValid(startingAnchor, endAnchor);
	}
	
	public DiagramState getState() {return state;}
	
	/**
	 * A diagram that is dirty is structurally out-of-sync with what is running
	 * in the gateway.
	 * @return true if the diagram does not represent what is actually running.
	 */
	public boolean isDirty() {return dirty;}
	
	public void setDirty(boolean dirty) {
		this.dirty = dirty;
		super.fireStateChanged();
	}
	
	@Override
	public void setDiagramSize(Dimension dim) {
		diagramSize = dim;
		super.fireStateChanged();  // Bypass setting block dirty
	}
	
	public void setEncapsulationBlockID(UUID encapsulationBlockID) {this.encapsulationBlockID = encapsulationBlockID;}
	
	public void setState(DiagramState state) {this.state = state;}
	
	/**
	 * There are a few situations (like deserialization) where we want to suppress the dirty propagation.
	 * This method is called because we've messed up the diagram with a structural change.
	 */
	@Override
	public void fireStateChanged() {
		if( !suppressStateChangeNotification ) {
			setDirty(true); // Fires super method which informs the listeners.
		}
	}
	/**
	 * Update the UI, including connections. The block class listens for property values.
	 */
	public void refresh() {
		NotificationHandler handler = NotificationHandler.getInstance();
		for( Connection cxn:connections) {
			BasicAnchorPoint bap = (BasicAnchorPoint)cxn.getOrigin();
			if( bap!=null ) {    // Is null when block-and-connector library is hosed.
				ProcessBlockView blk = (ProcessBlockView)bap.getBlock();
				String key = NotificationKey.keyForConnection(blk.getId().toString(), bap.getId().toString());
				handler.addNotificationChangeListener(key,CLSS, bap);
			}
		}
	}
	/**
	 * Create keyed listeners to process notifications from the Gateway.
	 * The listeners are very specific UI components that essentially
	 * update themselves.
	 */
	public void registerChangeListeners() {
		//log.infof("%s.registerChangeListeners: %s...",TAG,getName());
		NotificationHandler handler = NotificationHandler.getInstance();
		// Connections. Register the upstream anchors (merely a convention).
		// And while we're at it, update the connection state based on the latest
		// value from our newly deserialized diagram.
		for( Connection cxn:connections) {
			BasicAnchorPoint bap = (BasicAnchorPoint)cxn.getOrigin();
			if( bap!=null ) {    // Is null when block-and-connector library is hosed.
				ProcessBlockView blk = (ProcessBlockView)bap.getBlock();
				String key = NotificationKey.keyForConnection(blk.getId().toString(), bap.getId().toString());
				handler.addNotificationChangeListener(key,CLSS, bap);
			}
		}
		
		// Register any properties "bound" to the engine
		// It is the responsibility of the block to trigger
		// this as it evaluates. Update the value from the newly deserialized diagram.
		// Also register self for any block name changes
		for(ProcessBlockView block:blockMap.values() ) {
			block.startup();
			if( block.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE) ||
				block.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK)) {
				String key = NotificationKey.keyForBlockName(block.getId().toString());
				handler.addNotificationChangeListener(key,CLSS, block);
			}
		}
		// Register self for watermark changes
		String key = NotificationKey.watermarkKeyForDiagram(getId().toString());
		handler.addNotificationChangeListener(key,CLSS,this);
		
		// Finally tell the Gateway to report status - on everything
		// Theoretically this is overkill because the notification handler should know about all of this.
		if( appRequestHandler!=null) appRequestHandler.triggerStatusNotifications();
	}
	
	/**
	 * Presumably the diagram is closing. Remove the keyed listeners that were waiting 
	 * on notifications from the Gateway.
	 */
	public void unregisterChangeListeners() {
		//log.infof("%s.unregisterChangeListeners: ...",TAG);
		NotificationHandler handler = NotificationHandler.getInstance();
		// Connections. Un-register the upstream anchors (these are what was originally registered).
		for( Connection cxn:connections) {
			BasicAnchorPoint bap = (BasicAnchorPoint)cxn.getOrigin();
			ProcessBlockView blk = (ProcessBlockView)bap.getBlock();
			handler.removeNotificationChangeListener(NotificationKey.keyForConnection(blk.getId().toString(),bap.getId().toString()),CLSS);
			
		}
		
		// De-register any properties "bound" to the engine
		for(ProcessBlockView block:blockMap.values() ) {
			for(BlockProperty prop:block.getProperties()) {
				block.shutdown();
			}
			
			if( block.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE) ||
				block.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK)) {
				handler.removeNotificationChangeListener(NotificationKey.keyForBlockName(block.getId().toString()),block.getId().toString());
			}
		}
		// Finally, deregister self
		String key = NotificationKey.watermarkKeyForDiagram(getId().toString());
		handler.removeNotificationChangeListener(key,CLSS);
	}
	
	/**
	 * We've updated the block, now update the types of the downstream connections.
	 * @param block
	 * @param type
	 */
	public void updateConnectionTypes(ProcessBlockView block,ConnectionType type) {
		for(ProcessAnchorDescriptor pad:block.getAnchors()) {
			if( pad.getType().equals(AnchorType.Origin) &&
			    !pad.getConnectionType().equals(ConnectionType.SIGNAL)) {
				for(Connection cxn:connections) {
					BasicAnchorPoint bap = (BasicAnchorPoint)cxn.getOrigin();
					if( bap.getBlock().getId().equals(block.getId())) {
						bap.setConnectionType(type);
					}
				}
			}
				
		}
	}

	// This is called from the PropertyPanel editor and should be when a tag is dropped on a block
	// Validate business rules
	public String isValidBindingChange(ProcessBlockView pblock,BlockProperty prop,String tagPath, DataType type,Integer tagProp) {
		String msg = null;

		// ConType is the type of the proposed new connection.
		ConnectionType conType = pblock.determineDataTypeFromTagType(type);

		Collection<Connection> cxns = getConnections();
		for (Connection connection:cxns) {
			BasicAnchorPoint intended = null;
			if (connection.getOrigin().getBlock() == pblock) {
				intended = (BasicAnchorPoint)connection.getTerminus();
			}
			if (connection.getTerminus().getBlock() == pblock) {
				intended = (BasicAnchorPoint)connection.getOrigin();
			}

			if (intended != null && !intended.allowConnectionType(conType)) {
				msg = String.format("%s: Tag change error, cannot connect %s to %s", pblock.getName(),intended.getConnectionType().name(), conType.name());
			}
		}
		
		// If block is a Sink, there cannot be another Sink that references the same tag.
		// The tag cannot be a Boolean
		if(pblock.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ) {
			if(type.equals(DataType.Boolean)) {
				msg = String.format("Sink %s icannot be bound to a Boolean tag, Use a Text tag instead.",pblock.getName());
			}
			else {
				if(tagPath!=null && !tagPath.isEmpty() ) {
					List<SerializableBlockStateDescriptor> blocks = appRequestHandler.listBlocksForTag(tagPath);
					for(SerializableBlockStateDescriptor desc:blocks) {
						if( desc.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) &&
								!desc.getIdString().equals(pblock.getId().toString())) {
							msg = String.format("%s: cannot bind to same tag as sink %s", pblock.getName(),desc.getName());
							break;
						}
					}
				}
			}
		}
		// block binding to expressions for output
		else if( pblock.getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT)    &&
				prop.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
				tagProp != ExpressionType.None.getIntValue() ) {  // only update the tagpath property
			msg = "Unable to bind expression tag to output";
		}
		// block binding of Input/Output to tags in correction folder
		else if( (pblock.getClassName().equals(BlockConstants.BLOCK_CLASS_INPUT)    ||
				pblock.getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT))  &&
				prop.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
				BusinessRules.isStandardConnectionsFolder(tagPath) ) {  
			msg = "Input and outputs cannot be bound to tags in the connections folder";
		}
		// require sources and sinks be bound to tags in connections
		else if( (pblock.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)    ||
				pblock.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK))  &&
				prop.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
				!BusinessRules.isStandardConnectionsFolder(tagPath) ) {  
			msg = String.format("Sources and sinks must be bound to tags in %s",BlockConstants.SOURCE_SINK_TAG_FOLDER);
		}

		return msg;  // this could return an error message
	}
	
	// ------------------------------------------- NotificationChangeListener --------------------------------------
	/**
	 * Do nothing for a binding change - it just doesn't apply here
	 */
	@Override
	public void bindingChange(String pname,String binding) {
		log.infof("%s.bindingChange: %s binding = %s",CLSS,getName(),binding);
	} 
	@Override
	public void diagramStateChange(String path, String stateString) {
		log.infof("%s.diagramStateChange: %s state = %s",CLSS,path,stateString);
	}
	// Let the blocks subscribe to their own name changes
	@Override
	public void nameChange(String nm) {}
	@Override
	public void propertyChange(String pname,Object value) {}
	/**
	 * The value that we expect is a state change
	 */
	@Override
	public void valueChange(QualifiedValue value) {}

	@Override
	public void watermarkChange(String mark) {
		setWatermark(mark);
	}

}
