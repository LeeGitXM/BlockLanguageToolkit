/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.gateway.engine;

import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.ConnectionPostNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.ControllerRequestHandler;
import com.ils.blt.gateway.tag.TagListener;
import com.ils.common.BoundedBuffer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.tag.TagReader;
import com.ils.common.tag.TagUtility;
import com.ils.common.tag.TagValidator;
import com.ils.common.tag.TagWriter;
import com.ils.common.watchdog.AcceleratedWatchdogTimer;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.gateway.clientcomm.GatewaySessionManager;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  The block execution controller is responsible for the dynamic activity for the collection
 *  of diagrams. It receives status updates from the RPC controller and from the resource manager
 *  which is its delegate regarding model changes. The changes are analyzed to
 *  determine if one or more downstream blocks are to be informed of the change.
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class BlockExecutionController implements ExecutionController, Runnable {
	private final static String CLSS = "BlockExecutionController";
	private static BlockExecutionController instance = null;
	public final static String CONTROLLER_RUNNING_STATE = "running";
	public final static String CONTROLLER_STOPPED_STATE = "stopped";
	private static int BUFFER_SIZE = 100;       // Buffer Capacity
	private static int THREAD_POOL_SIZE = 10;   // Notification threads
	private final ILSLogger log;
	private GatewaySessionManager sessionManager = null;
	private ModelManager modelManager = null;
	private final WatchdogTimer watchdogTimer;
	private final AcceleratedWatchdogTimer secondaryWatchdogTimer;  // For isolated
	private final ExecutorService threadPool;
	private TagValidator tagValidator = null;

	// Cache the values for tag provider and database
	private String productionDatabase = null;
	private String isolationDatabase  = null;
	private String productionProvider = null;
	private String isolationProvider  = null;
	private double isolationTimeFactor= Double.NaN;

	private final BoundedBuffer buffer;
	private TagReader  tagReader = null;
	private final TagListener tagListener;    // Tag subscriber
	private TagWriter tagWriter = null;
	private Thread notificationThread = null;
	
	// Make this static so we can test without creating an instance.
	private static boolean stopped = true;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockExecutionController() {
		log = LogMaker.getLogger(getClass().getPackage().getName());
		this.threadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
		this.tagListener = new TagListener(this);
		this.buffer = new BoundedBuffer(BUFFER_SIZE);
		// Timers get started and stopped with the controller
		this.watchdogTimer = new WatchdogTimer("MainTimer");
		this.secondaryWatchdogTimer = new AcceleratedWatchdogTimer("SecondaryTimer");
		this.clearCache();
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static BlockExecutionController getInstance() {
		if( instance==null) {
			synchronized(BlockExecutionController.class) {
				instance = new BlockExecutionController();
			}
		}
		return instance;
	}
	
	/**
	 * Someone has injected a message into the system via broadcast
	 */
	@Override
	public void acceptBroadcastNotification(BroadcastNotification note) {
		log.tracef("%s.acceptBroadcastNotification: %s (%s) %s", CLSS,note.getDiagramId(),note.getSignal().getCommand(),
				(stopped?"REJECTED, controller stopped":""));
		ProcessDiagram diagram = getDiagram(note.getDiagramId());
		if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED)) {
			try {
				if(!stopped) buffer.put(note);
			}
			catch( InterruptedException ie ) {
				log.error("ERROR INJECTING SIGNAL MESSAGE ", ie);
				
			}
		}
	}
	
	/**
	 * A block has completed evaluation. A new value has been placed on its output.
	 * Place the notification into the queue for delivery to the appropriate downstream blocks.
	 * If we're stopped or the diagram is not active, these all go into the bit bucket.
	 */
	@Override
	public void acceptCompletionNotification(OutgoingNotification note) {
		if( log.isTraceEnabled() && note!=null && note.getValue()!=null ) {
			log.tracef("%s:acceptCompletionNotification: %s:%s = %s %s", CLSS,note.getBlock().getBlockId().toString(),note.getPort(),
				note.getValue().toString(),
				(stopped?"REJECTED, controller stopped":""));
		}
		ProcessDiagram diagram = getDiagram(note.getBlock().getParentId());
		if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED)) {
			try {
				if(!stopped) buffer.put(note);
			}
			catch( InterruptedException ie ) {}
		}
	}
	
	/**
	 * @param note the notification to be distributed to all connection posts
	 *        interested in the sender. Sender must be ACTIVE.
	 */
	@Override
	public void acceptConnectionPostNotification(ConnectionPostNotification note) {
		log.tracef("%s:acceptConnectionPostNotification: %s %s", CLSS,note.getOriginName(),
				(stopped?"REJECTED, controller stopped":""));
		ProcessDiagram diagram = getDiagram(note.getDiagramId());
		if( diagram!=null && diagram.getState().equals(DiagramState.ACTIVE)) {
			try {
				if(!stopped) buffer.put(note);
			}
			catch( InterruptedException ie ) {}
		}
	}

	/**
	 * Clear cached values to guarantee that next access forces a read from persistent storage.
	 */
	@Override
	public void clearCache() {
		productionDatabase = null;
		isolationDatabase  = null;
		productionProvider = null;
		isolationProvider  = null;
		isolationTimeFactor= Double.NaN;
	}
	@Override
	public String getIsolationDatabase() {
		if(isolationDatabase==null) {
			isolationDatabase = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE);
		}
		return isolationDatabase;
	}
	@Override
	public String getProductionDatabase() {
		if(productionDatabase==null) {
			productionDatabase = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE);
		}
		return productionDatabase;
	}
	@Override
	public String getIsolationProvider() {
		if(isolationProvider==null) {
			isolationProvider = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER);
		}
		return isolationProvider;
	}
	@Override
	public String getProductionProvider() {
		if(productionProvider==null) {
			productionProvider = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER);
		}
		return productionProvider;
	}
	/**
	 * NOTE: This value is a "speed-up" factor. .
	 * @return
	 */
	@Override
	public double getIsolationTimeFactor() {
		if(Double.isNaN(isolationTimeFactor) ) {
			String factor = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_TIME);
			if( factor.isEmpty() ) factor = "1.0";
			try {
				isolationTimeFactor = Double.parseDouble(factor);
			}
			catch(NumberFormatException nfe) {
				log.errorf("%s.getIsolationTimeFactor: Could not parse (%s) to a double value (%s)",CLSS,factor,nfe.getMessage());
				isolationTimeFactor = 1.0;
			}
		}
		return isolationTimeFactor;
	}
	
	/**
	 * Obtain the running state of the controller. This is a static method
	 * so that we don't have to instantiate an instance if there is none currently.
	 * @return the run state of the controller. ("running" or "stopped")
	 */
	public static String getExecutionState() {
		if( stopped ) return CONTROLLER_STOPPED_STATE;
		else          return CONTROLLER_RUNNING_STATE;
	}
	
	public WatchdogTimer getTimer() {return watchdogTimer;}
	public AcceleratedWatchdogTimer getSecondaryTimer() {return secondaryWatchdogTimer;}

	/**
	 * Start the controller, watchdogTimer, tagListener and TagWriter. We assume that projects are
	 * already loaded.
	 * @param context the gateway context
	 */
	public synchronized void start(GatewayContext context) {
		log.infof("%s: ============================ STARTED ===================================",CLSS);
		if(!stopped) return;  
		stopped = false;
		this.notificationThread = new Thread(this, "BlockExecutionController");
		notificationThread.setDaemon(true);
		notificationThread.start();
		watchdogTimer.start();
		// NOTE: The watchdog uses the reciprocal of this ...
		secondaryWatchdogTimer.setFactor(getIsolationTimeFactor());
		secondaryWatchdogTimer.start();
		sessionManager = context.getGatewaySessionManager();
		// Prepare tag readers/writers
		this.tagReader = new TagReader(context);
		this.tagValidator = new TagValidator(context);
		this.tagWriter = new TagWriter(context);
		// Activate all of the blocks in the diagram.
		modelManager.startBlocks();
		// Once blocks are started, start tag subscriptions
		tagListener.setTagReader(tagReader);
		tagListener.restartSubscriptions(context);
	}
	
	/**
	 * Stop the controller, watchdogTimer, tagListener and TagWriter. Set all
	 * instance values to null to, hopefully, allow garbage collection.
	 */
	public synchronized void stop() {
		log.infof("%s: ============================== STOPPING ===========================",CLSS);
		if(stopped) return;
		stopped = true;
		if(notificationThread!=null) {
			notificationThread.interrupt();
		}
		tagListener.stop();
		secondaryWatchdogTimer.stop();
		watchdogTimer.stop();
		// Shutdown all of the blocks in the diagram.
		modelManager.stopBlocks();
		log.infof("%s: STOPPED",CLSS);
	}
	public  void setDelegate(ModelManager resmgr) { this.modelManager = resmgr; }
	public void triggerStatusNotifications() {
		for( ProcessDiagram diagram:modelManager.getDiagrams()) {
			for(ProcessBlock block:diagram.getProcessBlocks()) {
				block.notifyOfStatus();
			}
		}
	}
	
	// ======================= Delegated to ModelManager ======================
	/**
	 * Add a temporary diagram that is not associated with a project resource. This
	 * diagram will not be persisted. Subscriptions are not activated at this point.
	 * @param diagram the diagram to be added to the engine.
	 */
	public void addTemporaryDiagram(ProcessDiagram diagram) {
		modelManager.addTemporaryDiagram(diagram);
	}

	public ProcessBlock getBlock(ProcessDiagram diagram,UUID blockId) {
		return modelManager.getBlock(diagram,blockId);
	}
	public ProcessBlock getBlock(long projectId,long resourceId,UUID blockId) {
		return modelManager.getBlock(projectId,resourceId,blockId);
	}
	public Connection getConnection(long projectId,long resourceId,String connectionId) {
		return modelManager.getConnection(projectId,resourceId,connectionId);
	}
	public ModelManager getDelegate() {
		return modelManager;
	}
	public ProcessDiagram getDiagram(ProjectResourceId resourceId) {
		return modelManager.getDiagram(resourceId);
	}
	@Override
	public ProcessDiagram getDiagram(String diagramIdString) {
		ProcessDiagram pd = null;
		if( diagramIdString!=null ) {
			UUID uuid = UUID.fromString(diagramIdString);
			if( uuid!=null ) {
				pd = getDiagram(uuid);
			} 
		}
		return pd;
	}

	// Tag change scripts fire before the modules are loaded ...
	public ProcessDiagram getDiagram(UUID id) {
		if( modelManager==null) {
			log.infof("%s.getDiagram: ERROR called before controller has been initialized",CLSS);
			return null;
		}
		return modelManager.getDiagram(id);
	}

	@Override
	public ProcessBlock getProcessBlock(String diagramId, String blockId) {
		ProcessBlock result = null;
		ProcessDiagram diagram = getDiagram(diagramId);
		if( diagram!=null ) {
			result = diagram.getBlock(UUID.fromString(blockId));
		}
		return result;
	}
	
	public ProcessNode getProcessNode(ProjectResourceId id) {
		return modelManager.getProcessNode(id);
	}
	public List<SerializableResourceDescriptor> getDiagramDescriptors() {
		return modelManager.getDiagramDescriptors();
	}
	public List<SerializableResourceDescriptor> getDiagramDescriptors(String projectName) {
		return modelManager.getDiagramDescriptors(projectName);
	}
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName) {
		ControllerRequestHandler handler = ControllerRequestHandler.getInstance();
		return handler.listBlocksConnectedAtPort(diagramId, blockId,portName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams) {
		return modelManager.listBlocksDownstreamOf(diagramId, blockId,spanDiagrams);
	}
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams) {
		return modelManager.listBlocksUpstreamOf(diagramId, blockId,spanDiagrams);
	}
	public List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName) {
		ControllerRequestHandler handler = ControllerRequestHandler.getInstance();
		return handler.listSinksForSource(diagramId, blockName);
	}
	public List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockName) {
		ControllerRequestHandler handler = ControllerRequestHandler.getInstance();
		return handler.listSourcesForSink(diagramId, blockName);
	}
	
	/**
	 * The node must be an element of the nav-tree, that is an application,
	 * family, folder or diagram. 
	 * @param nodeId
	 * @return colon-separated path to the indicated node
	 */
	public String pathForNode(UUID nodeId) {
		return modelManager.pathForNode(nodeId);
	}
	/**
	 * Execute a block's propagate method. This places the last-computed
	 * value on the output(s).
	 * @param diagramId the block or diagram identifier.
	 * @param blockId the block or diagram identifier.
	 */
	public void propagateBlockState(UUID diagramId,UUID blockId) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null) {
			ProcessBlock block = modelManager.getBlock(diagram, blockId);
			if( block!=null) block.propagate();
		}
	}
	/**
	 * Reset a block.
	 * @param diagramId the block or diagram identifier.
	 * @param blockId the block or diagram identifier.
	 */
	public void resetBlock(UUID diagramId,UUID blockId) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		ProcessBlock block = null;
		if( diagram!=null) {
			block = modelManager.getBlock(diagram, blockId);
			if( block!=null) {
				block.reset();
				log.debugf("%s.resetBlock: diagram:block %s:%s RESET",CLSS,diagram.getName(),block.getName());
			}
		}
		if(block==null) log.infof("%s.resetBlock: diagram:block %s:%s NOT FOUND",CLSS,diagramId.toString(),blockId.toString());
	}
	/**
	 * Reset all blocks on a diagram. Resetting blocks with
	 * truth-value outputs, propagates an UNKNOWN. This is 
	 * done by the blocks. After all of this, cause the input
	 * blocks to evaluate.
	 * @param diagramId the diagram identifier.
	 */
	public void resetDiagram(UUID diagramId) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null) {
			for(ProcessBlock block:diagram.getProcessBlocks() ) {
				block.reset();
			}
			// The blocks that delay start are those that propagate tag values.
			for(ProcessBlock block:diagram.getProcessBlocks() ) {
				if( block.delayBlockStart() ) block.evaluate();
			}
			log.debugf("%s.resetBlock: diagram %s RESET",CLSS,diagram.getName());
		}
		else {
			log.infof("%s.resetDiagram: diagram %s NOT FOUND",CLSS,diagramId.toString());
		}
	}

	public List<SerializableResourceDescriptor> queryControllerResources() {
		return modelManager.queryControllerResources();
	}
	/**
	 * Remove all diagrams and applications from the controller. 
	 * Before doing so, stop all subscriptions.
	 */
	public void removeAllDiagrams() {
		clearSubscriptions();
		modelManager.removeAllDiagrams();
	}
	/**
	 * Delete a temporary diagram that is not associated with a project resource. 
	 * Any subscriptions are de-activated before removal.
	 * @param Id the UUID of the diagram to be deleted from the engine.
	 */
	public void removeTemporaryDiagram(UUID Id) {
		modelManager.removeTemporaryDiagram(Id);
	}
	
	// ======================= Delegated to TagListener ======================
	/**
	 * Tell the tag listener to forget about any past subscriptions. By default,
	 * the listener will re-establish all previous subscriptions when restarted.
	 */
	@Override
	public void clearSubscriptions() {
		tagListener.clearSubscriptions();
	}
	@Override
	public String getSubscribedPath(ProcessBlock block,BlockProperty property) {
		return tagListener.getSubscribedPath(block,property);
	}
	@Override
	public QualifiedValue getTagValue(UUID diagramId,String path) {
		if( tagReader==null ) {
			log.warnf("%s.getTagValue: tagReader is null. Received read request for %s before controller was ready. Ignored.",CLSS,(path==null?"NULL":path));
			return null;
		}
		return tagReader.readTag(path);
	}
	@Override
	public boolean hasActiveSubscription(ProcessBlock block,BlockProperty property,String tagPath) {
		// If the block is disabled, report true (meaning not a problem)
		ProcessDiagram diagram = getDiagram(block.getParentId());
		boolean result = true;
		if( diagram !=null && !diagram.getState().equals(DiagramState.DISABLED)) {
			result = tagListener.hasActiveSubscription(block, property,tagPath);
		}
		return result;
	}
	/**
	 * Stop the tag subscription associated with a particular property of a block.
	 * There may be other entities still subscribed to the same tag.
	 */
	public void removeSubscription(ProcessBlock block,BlockProperty property) {
		if( property!=null && property.getBinding()!=null && 
			(	property.getBindingType()==BindingType.TAG_READ || 
				property.getBindingType()==BindingType.TAG_READWRITE ||
				property.getBindingType()==BindingType.TAG_MONITOR )  ) {
			String tagPath = property.getBinding().toString();
			if( tagPath!=null && tagPath.length()>0) {
				tagListener.removeSubscription(block,property,tagPath);
			}
		}
	}
	/**
	 * Start a subscription for a block attribute associated with a tag.
	 * Do nothing if block is disabled.
	 */
	public void startSubscription(DiagramState ds,ProcessBlock block,BlockProperty property) {
		if( ds.equals(DiagramState.DISABLED)|| block==null || property==null || 
				!(property.getBindingType().equals(BindingType.TAG_READ) || 
				  property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				  property.getBindingType().equals(BindingType.TAG_MONITOR) )   ) return;
		guaranteeBindingHasProvider(ds,property);
		String tagPath = property.getBinding();
		tagListener.defineSubscription(block,property,tagPath);
	}
	
	// ======================= Delegated to TagWriter ======================
	/**
	 * Write a value to a tag. If the diagram referenced diagram is disabled
	 * then this method has no effect.
	 * @param diagramId UUID of the parent diagram
	 * @param tagPath
	 * @param val
	 */
	public void updateTag(UUID diagramId,String tagPath,QualifiedValue val) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED)) {
			if(diagram.getState().equals(DiagramState.ACTIVE)) {
				String provider = getProductionProvider();
				tagPath = TagUtility.replaceProviderInPath(provider,tagPath);
			}
			else if(diagram.getState().equals(DiagramState.ISOLATED)) {
				String provider = getIsolationProvider();
				tagPath = TagUtility.replaceProviderInPath(provider,tagPath);
			}
			tagWriter.write(tagPath,val.getValue().toString(),val.getTimestamp().getTime());
		}
		else {
			log.infof("%s.updateTag %s REJECTED, diagram not active",CLSS,tagPath);
		}
	}
	
	/**
	 * Test the validity of a tag path. If the referenced diagram is disabled
	 * then this method has no effect. Otherwise we need to set the proper
	 * provider.
	 * @param diagramId UUID of the parent diagram
	 * @param tagPath
	 * @return reason else null if the path is valid
	 */
	public String validateTag(UUID diagramId,String tagPath) {
		String result = null;
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED) ) {
			result = tagValidator.validateTag(tagPath);
		}
		else {
			log.infof("%s.validateTag %s, parent diagram not found or disabled",CLSS,tagPath);
		}
		return result;
	}


	// ============================ Completion Handler =========================
	/**
	 * Wait for work to arrive at the output of a bounded buffer. The contents of the bounded buffer
	 * are OutgoingValueNotification objects. In/out are from the viewpoint of a block.
	 */
	public void run() {
		while( !stopped  ) {
			try {
				Object work = buffer.get();
				if( work instanceof OutgoingNotification) {
					OutgoingNotification inNote = (OutgoingNotification)work;
					// Query the diagram to find out what's next
					ProcessBlock pb = inNote.getBlock();
					if( log.isTraceEnabled() ) {
						log.tracef("%s.run: processing incoming note from %s:%s = %s", CLSS,pb.toString(),inNote.getPort(),inNote.getValue().toString());
					}
					// Send the push notification
					sendConnectionNotification(pb.getBlockId().toString(),inNote.getPort(),inNote.getValue());
					ProcessDiagram dm = modelManager.getDiagram(pb.getParentId());
					if( dm!=null && inNote!=null && inNote.getValue()!=null) {
						// Do not propagate UNSET (even a String value)
						if( (inNote.getValue().getValue() instanceof TruthValue && inNote.getValue().getValue().equals(TruthValue.UNSET)) ||
							(inNote.getValue().getValue() instanceof String && inNote.getValue().getValue().equals(TruthValue.UNSET.name()) )	)  {
							;
						}
						// Values that are signals are processed separately
						else if( inNote.getValue().getValue() instanceof Signal ) {
							Collection<SignalNotification> outgoing = dm.getOutgoingSignalNotifications(inNote);
							// It is common for display blocks, for example, to be left unconnected.
							// Don't get too worried about this.
							if( outgoing.isEmpty() ) log.debugf("%s: no downstream blocks found ...",CLSS);
							for(SignalNotification outNote:outgoing) {
									threadPool.execute(new IncomingBroadcastTask(outNote.getBlock(),outNote));
							}
						}
						else {
							Collection<IncomingNotification> outgoing = dm.getOutgoingNotifications(inNote);
							// It is common for display blocks, for example, to be left unconnected.
							// Don't get too worried about this.
							if( outgoing.isEmpty() ) log.debugf("%s: no downstream connections found ...",CLSS);
							for(IncomingNotification outNote:outgoing) {
								UUID outBlockId = outNote.getConnection().getTarget();
								ProcessBlock outBlock = dm.getBlock(outBlockId);
								if( outBlock!=null ) {
									log.tracef("%s.run: sending outgoing notification: to %s:%s = %s", CLSS,outBlock.toString(),
											  outNote.getConnection().getDownstreamPortName(),outNote.getValue().toString());
									threadPool.execute(new IncomingValueChangeTask(outBlock,outNote));
								}
								else {
									log.warnf("%s: run: target block %s not found in diagram map ",CLSS,outBlockId.toString());
								}
							}
						}
					}
					else {
						log.warnf("%s: run: diagram %s not found for value change notification",CLSS,pb.getParentId().toString());
					}
				}
				else if( work instanceof BroadcastNotification) {
					BroadcastNotification inNote = (BroadcastNotification)work;
					// Query the diagram to find out what's next. These are "unconnected" signals.
					ProcessDiagram dm = modelManager.getDiagram(inNote.getDiagramId());
					if( dm!=null) {
						log.debugf("%s.run: processing broadcast to diagram %s (%s)", CLSS,dm.getName(),inNote.getSignal().getCommand());
						Collection<SignalNotification> outgoing = dm.getBroadcastNotifications(inNote);
						if( outgoing.isEmpty() ) log.warnf("%s: no broadcast recipients found ...",CLSS);
						for(SignalNotification outNote:outgoing) {
							ProcessBlock outBlock = outNote.getBlock();
							log.debugf("%s.run: sending signal to %s", CLSS,outBlock.toString());
							threadPool.execute(new IncomingBroadcastTask(outBlock,outNote));
							
						}
					}
					// Note: This can legitimately happen if the diagram is deleted.
					else {
						log.warnf("%s.run: diagram %s not found in value change notification",CLSS,
								inNote.getDiagramId().toString());
					}
				}
				else {
					log.warnf("%s.run: Unexpected object in buffer (%s)",CLSS,work.getClass().getName());
				}
			}
			catch( InterruptedException ie) {}
		}
	}

	/**
	 * Notify any listeners in the Client or Designer scopes that a diagram has changed its alert state,
	 * presumably triggered due to a block state change on the diagram.
	 * @param resid resourceId of the diagram
	 * @param val new state (true implies alerting).
	 */
	@Override
	public void sendAlertNotification(ProjectResourceId resid, String val) {
		String key = NotificationKey.keyForAlert(resid);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendAlertNotification: No notification receiver for %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	/**
	 * Notify any notification listeners of changes to a block property. This is usually triggered by the 
	 * block itself. The ultimate receiver is typically a block property in the UI, a ProcessBlockView.
	 */
	@Override
	public void sendAuxDataNotification(String blkid,QualifiedValue val) {
		if( val==null ) return;    
		String key = NotificationKey.keyForAuxData(blkid);
		log.tracef("%s.sendAuxDataNotification: %s (%s)",CLSS,key,val.toString());
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendAuxDataNotification: Error transmitting %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	/**
	 * Notify any notification listeners of changes to a block property. This is usually triggered by the 
	 * block itself. The ultimate receiver is typically a block property in the UI, a ProcessBlockView.
	 */
	@Override
	public void sendPropertyNotification(String blkid, String propertyName,QualifiedValue val) {
		if( val==null ) return;    
		String key = NotificationKey.keyForProperty(blkid,propertyName);
		log.tracef("%s.sendPropertyNotification: %s (%s)",CLSS,key,val.toString());
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendPropertyNotification: Error transmitting %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	@Override
	public void sendNameChangeNotification(String blkid,String name) {
		String key = NotificationKey.keyForBlockName(blkid);
		log.tracef("%s.sendNameChangeNotification: %s (%s)",CLSS,key,name);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, name);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendPropertyBindingNotification: Error transmitting %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	/**
	 * Notify any notification listeners of changes to the binding of a block property. The most
	 * common case where this is used is when a tag provider is changed via a diagram state change.
	 */
	@Override
	public void sendPropertyBindingNotification(String blkid, String propertyName,String binding) {
		if( binding==null ) return;
		String key = NotificationKey.keyForPropertyBinding(blkid,propertyName);
		log.tracef("%s.sendPropertyBindingNotification: %s (%s)",CLSS,key,binding);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, binding);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendPropertyBindingNotification: Error transmitting %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	/**
	 * Notify any listeners in the Client or Designer scopes of the a change in the value carried by a connection.
	 * A connection is uniquely identified by a block and output port. The sender of this notification is the
	 * controller (this). The typical receiver is a BasicAnchorPoint embedded in a connection in the UI.
	 * @param blockid unique Id of the block
	 * @param port
	 * @param val
	 */
	@Override
	public void sendConnectionNotification(String blockid, String port, QualifiedValue val) {
		String key = NotificationKey.keyForConnection(blockid,port);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendConnectionNotification: No notification receiver for %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	/**
	 * Notify any listeners in the Client or Designer scopes that a diagram has changed state,
	 * presumably triggered from an external source.
	 * @param resourceId Id of the diagram
	 * @param val new state
	 */
	@Override
	public void sendStateNotification(ProjectResourceId resourceId, String val) {
		String key = NotificationKey.keyForDiagram(resourceId);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendStateNotification: No notification receiver for %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	/**
	 * Notify any listeners in the Client or Designer scopes that a diagram needs a different watermark. 
	 * A diagram is always saved from the designer, so it is not necessary to save this locally.
	 * @param diagramid unique Id of the diagram
	 * @param val new state
	 */
	@Override
	public void sendWatermarkNotification(String diagramid, String val) {
		String key = NotificationKey.watermarkKeyForDiagram(diagramid);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key,val );
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendWatermarkNotification: Error transmitting %s (%s)",CLSS,key,ex.getMessage());
		}
	}
	
	// This should be called only on properties with bindings
	private void guaranteeBindingHasProvider(DiagramState ds,BlockProperty bp) {
		String binding = bp.getBinding();
		if( binding!=null && binding.length()>0 && 
			!binding.startsWith("[") &&  !DiagramState.DISABLED.equals(ds) ) {
			// Add a provider
			String provider = null;
			if( DiagramState.ISOLATED.equals(ds)) {
				provider = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER);
			}
			else {
				provider = ControllerRequestHandler.getInstance().getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER);
			}
			bp.setBinding(String.format("[%s]%s", provider,binding));
		}		
	}

}
