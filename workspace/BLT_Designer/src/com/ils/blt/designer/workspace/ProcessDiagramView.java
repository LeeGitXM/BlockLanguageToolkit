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
	private static final String TAG = "ProcessDiagramView";
	private final Map<UUID,ProcessBlockView> blockMap = new HashMap<UUID,ProcessBlockView>();
	private List<Connection> connections = new ArrayList<Connection>();
	private Dimension diagramSize = new Dimension(800,600);
	private final UUID id;
	private final String name;
	private final long resourceId;
	private DiagramState state = DiagramState.ACTIVE;
	private DesignerContext context;
	private boolean dirty = false;   // A newly created diagram is "dirty" until it is saved
	                                 // but this looks better. It'll be dirty again with the first block
	
	/**
	 * Constructor: Create an instance given a SerializableDiagram
	 * @param resid
	 * @param diagram
	 */
	public ProcessDiagramView (long resid,SerializableDiagram diagram, DesignerContext context) {
		this(resid,diagram.getId(),diagram.getName());
		this.state = diagram.getState();
		this.context = context;
		for( SerializableBlock sb:diagram.getBlocks()) {
			ProcessBlockView pbv = new ProcessBlockView(sb);
			blockMap.put(sb.getId(), pbv);
			log.warnf("%s: createDiagramView: Added %s to map",TAG,sb.getId().toString());
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
						log.warnf("%s: createDiagramView: Failed to find block %s for begin anchor point %s",TAG,a.getParentId(),a);
					}
					if( blockb==null ) {
						log.warnf("%s: createDiagramView: Failed to find block %s for end anchor point %s",TAG,b.getParentId(),b);
					}
				}
			}
			else {
				log.warnf("%s: createDiagramView: Connection %s missing one or more anchor points",TAG,scxn.toString());
			}
			// Do this at the end to override state change on adding blocks.
			this.dirty = diagram.isDirty(); 
		}
	}
	public ProcessDiagramView(long resId,UUID uuid, String nam) {
		this.id = uuid;
		this.resourceId = resId;
		this.name = nam;
	}
	
	/** Get the current block property values from the Gateway. 
	 * If the block does not have a Gateway counterpart (e.g. diagram is dirty), we'll get the 
	 * default property list for the block class.
	 */
	private void initBlockProperties(ProcessBlockView block) {
		Collection<BlockProperty> propertyList;
		propertyList = new ArrayList<BlockProperty>();
		ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
		BlockProperty[] properties = handler.getBlockProperties(block.getClassName(),context.getProject().getId(),resourceId,block.getId());
		for(BlockProperty property:properties) {
			propertyList.add(property);
		}
		log.infof("%s.initBlockProperties - initialize property list for %s (%d properties)",TAG,block.getId().toString(),propertyList.size());
		block.setProperties(propertyList);
	}

	/**
	 * At the time that we add a new block, make sure that the block has a unique name.
	 *
	 * @param context 
	 */
	@Override
	public void addBlock(Block blk) {
		if( blk instanceof ProcessBlockView) {
			ProcessBlockView block = (ProcessBlockView) blk;
			initBlockProperties(block);
			log.infof("%s.addBlock - %s",TAG,block.getClassName());
			blockMap.put(blk.getId(), block);
			fireStateChanged();
		}
	}
	
	@Override
	public void addConnection(AnchorPoint begin, AnchorPoint end) {
		connections.add(new LookupConnection(this,begin,end));
		fireStateChanged();

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
		blockMap.remove(blk.getId());
		fireStateChanged();
	}
	@Override
	public void deleteConnection(AnchorPoint begin, AnchorPoint end) {
		for(Connection cxn:connections) {
			if( cxn.getOrigin()==begin && cxn.getTerminus()==end) {
				connections.remove(cxn);
				fireStateChanged();
				break;
			}
		}
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
	public Collection<Connection> getConnections() {return connections;}

	@Override
	public String getDiagramName() {return name;}

	@Override
	public Dimension getDiagramSize() {
		return diagramSize;
	}

	public UUID getId() {return id;}

	public String getName() {return name;}

	@Override
	public long getResourceId() {
		return resourceId;
	}

	// Specify whether or not a drop point is valid.
	@Override
	public boolean isConnectionValid(AnchorPoint startingAnchor, AnchorPoint endAnchor) {
		return true;
	}
	
	public DiagramState getState() {return state;}
	public boolean isDirty() {return dirty;}
	public void setDirty(boolean dirty) {this.dirty = dirty;}
	
	@Override
	public void setDiagramSize(Dimension dim) {
		diagramSize = dim;
		super.fireStateChanged();  // Bypass setting block dirty
	}
	public void setState(DiagramState state) {this.state = state;}
	
	@Override
	public void fireStateChanged() {
		setDirty(true);
		super.fireStateChanged();
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
			ProcessBlockView blk = (ProcessBlockView)bap.getBlock();
			String key = NotificationKey.keyForConnection(blk.getId().toString(), bap.getId().toString());
			log.infof("%s.registerChangeListeners: adding %s",TAG,key);
			handler.addNotificationChangeListener(key, bap);
		}
		
		// Register any properties "bound" to the engine
		// It is the responsibility of the block to trigger
		// this as it evaluates.
		for(ProcessBlockView block:blockMap.values() ) {
			for(BlockProperty prop:block.getProperties()) {
				if( prop.getBindingType().equals(BindingType.ENGINE)) {
					String key = NotificationKey.keyForProperty(block.getId().toString(), prop.getName());
					log.infof("%s.registerChangeListeners: adding %s",TAG,key);
					handler.addNotificationChangeListener(key, prop);
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
			handler.removeNotificationChangeListener(NotificationKey.keyForConnection(blk.getId().toString(), bap.getId().toString()));
		}
		
		// Register any properties "bound" to the engine
		// It is the responsibility of the block to trigger
		// this as it evaluates.
		for(ProcessBlockView block:blockMap.values() ) {
			for(BlockProperty prop:block.getProperties()) {
				if( prop.getBindingType().equals(BindingType.ENGINE)) {
					handler.removeNotificationChangeListener(NotificationKey.keyForProperty(block.getId().toString(), prop.getName()));
				}
			}
		}
	}
}
