package com.ils.blt.designer.workspace;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.common.serializable.SerializableAnchorPoint;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.inductiveautomation.ignition.common.util.AbstractChangeable;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.LookupConnection;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This class represents a diagram in the designer.
 */
public class ProcessDiagramView extends AbstractChangeable implements BlockDiagramModel {
	private static LoggerEx log = LogUtil.getLogger(ProcessDiagramView.class.getPackage().getName());
	// Use TAG as the "source" identifier when registering for notifications from Gateway
	private static final String TAG = "ProcessDiagramView";
	private final Map<UUID,ProcessBlockView> blockMap = new HashMap<UUID,ProcessBlockView>();
	private List<Connection> connections = new ArrayList<Connection>();
	private Dimension diagramSize = new Dimension(800,600);
	private final UUID id;
	private String name = "UNSET";
	private UUID encapsulationBlockID = null;  // Used only if this diagram represents a sub-workspace
	private final long resourceId;
	private DiagramState state = DiagramState.ACTIVE;
	private DesignerContext context;
	private boolean dirty = false;   // A newly created diagram is "dirty" until it is saved
	                                 // but this looks better. It'll be dirty again with the first block
	private boolean suppressStateChangeNotification = false;
	
	/**
	 * Constructor: Create an instance given a SerializableDiagram
	 * @param resid
	 * @param diagram
	 */
	public ProcessDiagramView (long resid,SerializableDiagram diagram, DesignerContext context) {
		this(resid,diagram.getId(),diagram.getName());
		this.state = diagram.getState();
		this.context = context;
		suppressStateChangeNotification = true;
		synchronized(this) {
			for( SerializableBlock sb:diagram.getBlocks()) {
				ProcessBlockView pbv = new ProcessBlockView(sb);
				blockMap.put(sb.getId(), pbv);
				log.debugf("%s.createDiagramView: Added %s to map",TAG,sb.getId().toString());
				this.addBlock(pbv);
			}

			for( SerializableConnection scxn:diagram.getConnections() ) {
				SerializableAnchorPoint a = scxn.getBeginAnchor();
				SerializableAnchorPoint b = scxn.getEndAnchor();
				if( a!=null && b!=null ) {
					ProcessBlockView blocka = blockMap.get(a.getParentId());
					ProcessBlockView blockb = blockMap.get(b.getParentId());
					if( blocka!=null && blockb!=null) {
						AnchorPoint origin = new ProcessAnchorView(blocka,a);
						AnchorPoint terminus = new ProcessAnchorView(blockb,b);
						this.addConnection(origin,terminus);   // AnchorPoints
					}
					else {
						if( blocka==null ) {
							log.warnf("%s.createDiagramView: Failed to find block %s for begin anchor point %s",TAG,a.getParentId(),a);
						}
						if( blockb==null ) {
							log.warnf("%s.createDiagramView: Failed to find block %s for end anchor point %s",TAG,b.getParentId(),b);
						}
					}
				}
				else {
					log.warnf("%s.createDiagramView: Connection %s missing one or more anchor points",TAG,scxn.toString());
				}
			}
		}
		suppressStateChangeNotification = false;
		// Do this at the end to override state change on adding blocks/connectors.
		// Note this shouldn't represent a change for parents
		this.dirty = diagram.isDirty();
	}
	
	public ProcessDiagramView(long resId,UUID uuid, String nam) {
		this.id = uuid;
		this.resourceId = resId;
		this.name = nam;
		log.infof("%s.ProcessDiagramView: %s CONSTRUCTOR",TAG,name);
	}
	
	/** Get the current block property values from the Gateway. 
	 * If the block does not have a Gateway counterpart (e.g. diagram is dirty), we'll get the 
	 * default property list for the block class.
	 */
	public void initBlockProperties(ProcessBlockView block) {
		Collection<BlockProperty> propertyList;
		propertyList = new ArrayList<BlockProperty>();
		ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
		BlockProperty[] properties = handler.getBlockProperties(block.getClassName(),context.getProject().getId(),resourceId,block.getId());
		for(BlockProperty property:properties) {
			propertyList.add(property);
		}
		log.tracef("%s.initBlockProperties - initialize property list for %s (%d properties)",TAG,block.getId().toString(),propertyList.size());
		block.setProperties(propertyList);
	}

	/**
	 * At the time that we add a new block, make sure that the block has a unique name.
	 * If it has no properties (e.g. was created from palette), then attempt to get 
	 * properties from its gateway counterpart. A newly imported block should already
	 * have properties.
	 * 
	 * @param context 
	 */
	@Override
	public void addBlock(Block blk) {
		if( blk instanceof ProcessBlockView) {
			ProcessBlockView block = (ProcessBlockView) blk;
			if( ((ProcessBlockView) blk).getProperties().isEmpty() ) initBlockProperties(block);
			log.tracef("%s.addBlock - %s",TAG,block.getClassName());
			blockMap.put(blk.getId(), block);
			fireStateChanged();
		}
	}
	
	@Override
	public void addConnection(AnchorPoint begin, AnchorPoint end) {
		if( begin!=null && end!=null ) {
			connections.add(new LookupConnection(this,begin,end));
			fireStateChanged();
		}
		else {
			log.warnf("%s.addConnection - rejected attempt to add a connection with null anchor",TAG);
		}

	}
	// NOTE: This does not set connection type
	private SerializableConnection convertConnectionToSerializable(Connection cxn) {
		SerializableConnection result = new SerializableConnection();
		if( cxn.getOrigin()!=null && cxn.getTerminus()!=null ) {	
			result.setBeginBlock(cxn.getOrigin().getBlock().getId()); 
			result.setEndBlock(cxn.getTerminus().getBlock().getId());
			result.setBeginAnchor(createSerializableAnchorPoint(cxn.getOrigin()));
			result.setEndAnchor(createSerializableAnchorPoint(cxn.getTerminus()));
		}
		else {
			log.warnf("%s.convertConnectionToSerializable: connection missing terminus or origin (%s)",TAG,cxn.getClass().getName());
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
				boolean found = false;
				for(ProcessAnchorDescriptor desc:beginBlock.getAnchors()) {
					if( desc.getDisplay().equalsIgnoreCase(port) ) {
						found = true;
						scxn.setType(desc.getConnectionType());
					}
				}
				if( !found ) log.warnf("%s.createSerializableRepresentation: unable to find %s port in begin block",TAG,port);
			}
			else {
				log.warnf("%s.createSerializableRepresentation: begin block lookup failed",TAG);
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
		if(!success) log.warnf("%s.deleteConnection: failed to find match to existing",TAG);
	}
	/**
	 * @return a background color appropriate for the current state
	 *         of the diagram
	 */
	public Color getBackgroundColorForState() {
		Color result = BLTProperties.DIAGRAM_ACTIVE_BACKGROUND;
		if( getState().equals(DiagramState.RESTRICTED)) result = BLTProperties.DIAGRAM_RESTRICTED_BACKGROUND;
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
		// TODO Auto-generated method stub
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

	@Override
	public long getResourceId() {
		return resourceId;
	}


	@Override
	public boolean isConnectionValid(AnchorPoint startingAnchor, AnchorPoint endAnchor) {
		return ProcessBlockView.isConnectionValid(startingAnchor, endAnchor);
	}
	
	public DiagramState getState() {return state;}
	/**
	 * A diagram that is dirty is structurally out-of-sync with what is running
	 * in the gateway.
	 * @return tue if the diagram does not represent what is actually running.
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
			NodeStatusManager statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
			statusManager.incrementDirtyNodeCount(getResourceId());
			setDirty(true); // Fires super method which informs the listeners.
		}
	}
	
	/**
	 * Create keyed listeners to process notifications from the Gateway.
	 * The listeners are very specific UI components that essentially
	 * update themselves.
	 */
	public void registerChangeListeners() {
		log.infof("%s.registerChangeListeners: ...",TAG);
		NotificationHandler handler = NotificationHandler.getInstance();
		// Connections. Register the upstream anchors (merely a convention).
		for( Connection cxn:connections) {
			BasicAnchorPoint bap = (BasicAnchorPoint)cxn.getOrigin();
			if( bap!=null ) {    // Is null when block-and-connector library is hosed.
				ProcessBlockView blk = (ProcessBlockView)bap.getBlock();
				String key = NotificationKey.keyForConnection(blk.getId().toString(), bap.getId().toString());
				log.tracef("%s.registerChangeListeners: adding %s",TAG,key);
				handler.addNotificationChangeListener(key,TAG, bap);
			}
		}
		
		// Register any properties "bound" to the engine
		// It is the responsibility of the block to trigger
		// this as it evaluates.
		for(ProcessBlockView block:blockMap.values() ) {
			for(BlockProperty prop:block.getProperties()) {
				if( prop.getBindingType().equals(BindingType.ENGINE)) {
					String key = NotificationKey.keyForProperty(block.getId().toString(), prop.getName());
					log.tracef("%s.registerChangeListeners: adding %s(%d)",TAG,key,prop.hashCode());
					handler.addNotificationChangeListener(key,TAG, prop);
					prop.addChangeListener(block);
				}
			}
		}
	}
	
	/**
	 * Presumably the diagram is closing. Remove the keyed listeners that were waiting 
	 * on notifications from the Gateway.
	 */
	public void unregisterChangeListeners() {
		log.infof("%s.unregisterChangeListeners: ...",TAG);
		NotificationHandler handler = NotificationHandler.getInstance();
		// Connections. Register the upstream anchors (merely a convention).
		for( Connection cxn:connections) {
			BasicAnchorPoint bap = (BasicAnchorPoint)cxn.getOrigin();
			ProcessBlockView blk = (ProcessBlockView)bap.getBlock();
			handler.removeNotificationChangeListener(NotificationKey.keyForConnection(blk.getId().toString(),bap.getId().toString()),TAG);
		}
		
		// Register any properties "bound" to the engine
		// It is the responsibility of the block to trigger
		// this as it evaluates.
		for(ProcessBlockView block:blockMap.values() ) {
			for(BlockProperty prop:block.getProperties()) {
				if( prop.getBindingType().equals(BindingType.ENGINE)) {
					handler.removeNotificationChangeListener(NotificationKey.keyForProperty(block.getId().toString(), prop.getName()),TAG);
					prop.removeChangeListener(block);
				}
			}
		}
	}
}
