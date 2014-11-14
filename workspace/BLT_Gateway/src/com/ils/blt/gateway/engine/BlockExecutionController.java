/**
 *   (c) 2014  ILS Automation. All rights reserved.
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
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.ConnectionPostNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.BoundedBuffer;
import com.ils.common.watchdog.Watchdog;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
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
	private final static String TAG = "BlockExecutionController";
	public final static String CONTROLLER_RUNNING_STATE = "running";
	public final static String CONTROLLER_STOPPED_STATE = "stopped";
	private static int BUFFER_SIZE = 100;       // Buffer Capacity
	private static int THREAD_POOL_SIZE = 10;   // Notification threads
	private final LoggerEx log;
	private GatewaySessionManager sessionManager = null;
	private ModelManager modelManager = null;
	private WatchdogTimer watchdogTimer = null;
	private static BlockExecutionController instance = null;
	private final ExecutorService threadPool;


	private final BoundedBuffer buffer;
	private final TagListener tagListener;    // Tag subscriber
	private final TagWriter tagWriter;
	private Thread notificationThread = null;
	// Make this static so we can test without creating an instance.
	private static boolean stopped = true;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockExecutionController() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.threadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
		this.tagListener = new TagListener(this);
		this.tagWriter = new TagWriter();
		this.buffer = new BoundedBuffer(BUFFER_SIZE);
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
		log.tracef("%s.acceptBroadcastNotification: %s (%s) %s", TAG,note.getDiagramId(),note.getSignal().getCommand(),
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
	 * A block has completed evaluation. A new value has been placed on its output.
	 * Place the notification into the queue for delivery to the appropriate downstream blocks.
	 * If we're stopped or the diagram is not active, these all go into the bit bucket.
	 */
	@Override
	public void acceptCompletionNotification(OutgoingNotification note) {
		log.tracef("%s:acceptCompletionNotification: %s:%s = %s %s", TAG,note.getBlock().getBlockId().toString(),note.getPort(),
				note.getValue().toString(),
				(stopped?"REJECTED, controller stopped":""));
		ProcessDiagram diagram = getDiagram(note.getBlock().getParentId());
		if( diagram!=null && diagram.getState().equals(DiagramState.ACTIVE)) {
			try {
				if(!stopped) buffer.put(note);
			}
			catch( InterruptedException ie ) {}
		}
	}
	
	/**
	 * @param note the notification to be distributed to all connection posts
	 *        interested in the sender
	 */
	@Override
	public void acceptConnectionPostNotification(ConnectionPostNotification note) {
		log.tracef("%s:acceptConnectionPostNotification: %s %s", TAG,note.getOriginName(),
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
	 * Change a tag subscription for a block's property. We assume that the property
	 * has been updated and contains the new path.
	 */
	@Override
	public void alterSubscription(UUID diagramId,UUID blockId,String propertyName) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null) {
			ProcessBlock block = diagram.getBlock(blockId);
			if( block!=null ) {
				BlockProperty bp = block.getProperty(propertyName);
				if( bp!=null ) {
					tagListener.removeSubscription(block,bp);
					startSubscription(block,bp);
				}
			}
		}
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
	
	/**
	 * Start the controller, watchdogTimer, tagListener and TagWriter.
	 * @param ctxt the gateway context
	 */
	public synchronized void start(GatewayContext context) {
		log.infof("%s: STARTED",TAG);
		if(!stopped) return;  
		stopped = false;
		tagListener.start(context);
		tagWriter.initialize(context);
		this.notificationThread = new Thread(this, "BlockExecutionController");
		log.debugf("%s START - notification thread %d ",TAG,notificationThread.hashCode());
		notificationThread.setDaemon(true);
		notificationThread.start();
		// Create a new watchdog timer each time we start the controller
		// It is started on creation
		watchdogTimer = new WatchdogTimer();
		sessionManager = context.getGatewaySessionManager();
		
		// Activate all of the blocks in the diagram.
		modelManager.startBlocks();
	}
	
	/**
	 * Stop the controller, watchdogTimer, tagListener and TagWriter. Set all
	 * instance values to null to, hopefully, allow garbage collection.
	 */
	public synchronized void stop() {
		log.infof("%s: STOPPING ...",TAG);
		if(stopped) return;
		stopped = true;
		if(notificationThread!=null) {
			notificationThread.interrupt();
		}
		tagListener.stop();
		watchdogTimer.stop();
		watchdogTimer = null;
		// Shutdown all of the blocks in the diagram.
		modelManager.stopBlocks();
		log.infof("%s: STOPPED",TAG);
	}
	
	public  void setDelegate(ModelManager resmgr) { this.modelManager = resmgr; }
	
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
	public ProcessDiagram getDiagram(long projectId,long resourceId) {
		return modelManager.getDiagram(projectId,resourceId);
	}
	public ProcessDiagram getDiagram(UUID id) {
		return modelManager.getDiagram(id);
	}
	public ProcessDiagram getDiagram(String projectName,String diagramPath) {
		return modelManager.getDiagram(projectName,diagramPath);
	}
	public List<String> getDiagramTreePaths(String projectName) {
		return modelManager.getDiagramTreePaths(projectName);
	}
	/**
	 * Reset a block.
	 * @param diagramId the block or diagram identifier.
	 * @param blockId the block or diagram identifier.
	 */
	public void resetBlock(UUID diagramId,UUID blockId) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null) {
			ProcessBlock block = modelManager.getBlock(diagram, blockId);
			if( block!=null) block.reset();
			
		}
	}
	/**
	 * Reset all blocks on a diagram.
	 * @param diagramId the diagram identifier.
	 */
	public void resetDiagram(UUID diagramId) {
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null) {
			for(ProcessBlock block:diagram.getProcessBlocks() ) {
				block.reset();
			}
		}
	}
	public void resetDiagram(String projectName,String diagramPath) {
		ProcessDiagram diagram = modelManager.getDiagram(projectName,diagramPath);
		for(ProcessBlock block:diagram.getProcessBlocks() ) {
			block.reset();
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
	/**
	 * Stop the tag subscription associated with a particular property of a block.
	 * There may be other entities still subscribed to the same tag.
	 */
	public void removeSubscription(ProcessBlock block,BlockProperty property) {
		if( property!=null && property.getValue()!=null && 
			(	property.getBindingType()==BindingType.TAG_READ || 
				property.getBindingType()==BindingType.TAG_READ ||
				property.getBindingType()==BindingType.TAG_MONITOR )  ) {
			String tagPath = property.getValue().toString();
			if( tagPath!=null && tagPath.length()>0) {
				tagListener.removeSubscription(block,property,tagPath);
			}
		}
	}
	/**
	 * Start a subscription for a block attribute associated with a tag.
	 */
	public void startSubscription(ProcessBlock block,BlockProperty property) {
		tagListener.defineSubscription(block, property);
	}
	
	// ======================= Delegated to TagWriter ======================
	/**
	 * Write a value to a tag. If the diagram referenced diagram is disabled
	 * or "restricted", then this method has no effect.
	 * @param diagramId UUID of the parent diagram
	 * @param path
	 * @param val
	 */
	public void updateTag(UUID diagramId,String path,QualifiedValue val) {
		log.debugf("%s.updateTag %s = %s ",TAG,path,val.toString());
		ProcessDiagram diagram = modelManager.getDiagram(diagramId);
		if( diagram!=null && diagram.getState().equals(DiagramState.ACTIVE)) {
			tagWriter.updateTag(diagram.getProjectId(),path,val);
		}
		else {
			log.infof("%s.updateTag REJECTED, diagram not active",TAG);
		}
	}
	// ======================= Delegated to Watchdog ======================
	/**
	 * "pet" a watch dog. The watch dog must be updated to expire some time 
	 * in the future. This method may also be used to insert a watch dog
	 * into the timer list for the first time.
	 * 
	 * This method has no effect unless the controller is running.
	 * @param pet the watchdog to stroke.
	 */
	public void pet(Watchdog dog) {
		if(watchdogTimer!=null) watchdogTimer.updateWatchdog(dog);
	}
	/**
	 * Remove a watch dog. Delete it from the list.
	 */
	public void removeWatchdog(Watchdog dog) {
		if(watchdogTimer!=null) watchdogTimer.removeWatchdog(dog);
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
					log.tracef("%s.run: processing incoming note from %s:%s = %s", TAG,pb.toString(),inNote.getPort(),inNote.getValue().toString());
					// Send the push notification
					sendConnectionNotification(pb.getBlockId().toString(),inNote.getPort(),inNote.getValue());
					ProcessDiagram dm = modelManager.getDiagram(pb.getParentId());
					if( dm!=null) {
						Collection<IncomingNotification> outgoing = dm.getOutgoingNotifications(inNote);
						// It is common for display blocks, for example, to be left unconnected.
						// Don't get too worried about this.
						if( outgoing.isEmpty() ) log.debugf("%s: no downstream connections found ...",TAG);
						for(IncomingNotification outNote:outgoing) {
							UUID outBlockId = outNote.getConnection().getTarget();
							ProcessBlock outBlock = dm.getBlock(outBlockId);
							if( outBlock!=null ) {
								log.tracef("%s.run: sending outgoing notification: to %s:%s = %s", TAG,outBlock.toString(),
										  outNote.getConnection().getDownstreamPortName(),outNote.getValue().toString());
								threadPool.execute(new IncomingValueChangeTask(outBlock,outNote));
							}
							else {
								log.warnf("%s: run: target block %s not found in diagram map ",TAG,outBlockId.toString());
							}
						}
					}
					else {
						log.warnf("%s: run: diagram %s not found for value change notification",TAG,pb.getParentId().toString());
					}
				}
				else if( work instanceof BroadcastNotification) {
					BroadcastNotification inNote = (BroadcastNotification)work;
					
					// Query the diagram to find out what's next. The diagramId is the resourceId
					ProcessDiagram dm = modelManager.getDiagram(inNote.getDiagramId());
					if( dm!=null) {
						log.debugf("%s.run: processing broadcast to diagram %s (%s)", TAG,dm.getName(),inNote.getSignal().getCommand());
						Collection<SignalNotification> outgoing = dm.getBroadcastNotifications(inNote);
						if( outgoing.isEmpty() ) log.warnf("%s: no broadcast recipients found ...",TAG);
						for(SignalNotification outNote:outgoing) {
							ProcessBlock outBlock = outNote.getBlock();
							log.debugf("%s.run: sending signal to %s", TAG,outBlock.toString());
							threadPool.execute(new IncomingBroadcastTask(outBlock,outNote));
							
						}
					}
					else {
						log.warnf("%s.run: diagram %s not found in value change notification",TAG,
								inNote.getDiagramId().toString());
					}
				}
				else {
					log.warnf("%s.run: Unexpected object in buffer (%s)",TAG,work.getClass().getName());
				}
			}
			catch( InterruptedException ie) {}
		}
	}

	/**
	 * Notify any notification listeners of changes to a block property. This is usually triggered by the 
	 * block itself. The ultimate receiver is typically a block property in the UI in a ProcessBlockView.
	 */
	@Override
	public void sendPropertyNotification(String blkid, String propertyName,QualifiedValue val) {
		String key = NotificationKey.keyForProperty(blkid,propertyName);
		log.tracef("%s.sendPropertyNotification: %s (%s)",TAG,key,val.toString());
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendPropertyNotification: Error transmitting %s (%s)",TAG,key,ex.getMessage());
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
	private void sendConnectionNotification(String blockid, String port, QualifiedValue val) {
		String key = NotificationKey.keyForConnection(blockid,port);
		try {
			sessionManager.sendNotification(ApplicationScope.DESIGNER, BLTProperties.MODULE_ID, key, val);
		}
		catch(Exception ex) {
			// Probably no receiver registered. This is to be expected if the designer is not running.
			log.debugf("%s.sendConnectionNotification: Error transmitting %s (%s)",TAG,key,ex.getMessage());
		}
	}

}
