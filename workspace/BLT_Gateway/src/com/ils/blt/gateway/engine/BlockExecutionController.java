/**
 *   (c) 2012  ILS Automation. All rights reserved.
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

import com.ils.block.ProcessBlock;
import com.ils.block.common.BindingType;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.BroadcastNotification;
import com.ils.block.control.ExecutionController;
import com.ils.block.control.IncomingNotification;
import com.ils.block.control.OutgoingNotification;
import com.ils.block.control.SignalNotification;
import com.ils.common.BoundedBuffer;
import com.ils.common.watchdog.Watchdog;
import com.ils.common.watchdog.WatchdogTimer;
import com.ils.connection.Connection;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
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
	private static int BUFFER_SIZE = 100;   // Buffer Capacity
	private final LoggerEx log;
	private ModelManager modelManager = null;
	private final WatchdogTimer watchdogTimer;
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
		this.threadPool = Executors.newFixedThreadPool(10);
		this.tagListener = new TagListener();
		this.tagWriter = new TagWriter();
		this.buffer = new BoundedBuffer(BUFFER_SIZE);
		this.watchdogTimer = new WatchdogTimer();
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
		log.infof("%s.acceptBroadcastNotification: %s (%s) %s", TAG,note.getDiagramId(),note.getSignal().getCommand(),
				(stopped?"REJECTED, stopped":""));
		try {
			if(!stopped) buffer.put(note);
		}
		catch( InterruptedException ie ) {}
	}
	
	/**
	 * A block has completed evaluation. A new value has been placed on its output.
	 * If we're stopped, these all go into the bit bucket.
	 */
	public void acceptCompletionNotification(OutgoingNotification note) {
		log.infof("%s:acceptCompletionNotification: %s:%s %s", TAG,note.getBlock().getBlockId().toString(),note.getPort(),
				(stopped?"REJECTED, stopped":""));
		try {
			if(!stopped) buffer.put(note);
		}
		catch( InterruptedException ie ) {}
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
		log.debugf("%s: STARTED",TAG);
		if(!stopped) return;  
		stopped = false;
		tagListener.start(context);
		tagWriter.start(context);
		this.notificationThread = new Thread(this, "BlockExecutionController");
		log.debugf("%s START - notification thread %d ",TAG,notificationThread.hashCode());
		notificationThread.setDaemon(true);
		notificationThread.start();
		watchdogTimer.start();
	}
	
	/**
	 * Stop the controller, watchdogTimer, tagListener and TagWriter. Set all
	 * instance values to null to, hopefully, allow garbage collection.
	 */
	public synchronized void stop() {
		log.debugf("%s: STOPPED",TAG);
		if(stopped) return;
		stopped = true;
		if(notificationThread!=null) {
			notificationThread.interrupt();
		}
		tagListener.stop();
		watchdogTimer.stop();
	}
	
	public  void setDelegate(ModelManager resmgr) { this.modelManager = resmgr; }
	
	// ======================= Delegated to ModelManager ======================
	public ProcessBlock getBlock(long projectId,long resourceId,UUID blockId) {
		return modelManager.getBlock(projectId,resourceId,blockId);
	}
	public Connection getConnection(long projectId,long resourceId,String connectionId) {
		return modelManager.getConnection(projectId,resourceId,connectionId);
	}
	public ProcessDiagram getDiagram(long projectId,long resourceId) {
		return modelManager.getDiagram(projectId,resourceId);
	}
	public ProcessDiagram getDiagram(String projectName,String diagramPath) {
		return modelManager.getDiagram(projectName,diagramPath);
	}
	public List<String> getDiagramTreePaths(String projectName) {
		return modelManager.getDiagramTreePaths(projectName);
	}
	
	// ======================= Delegated to TagListener ======================
	/**
	 * Start a subscription for a block attribute associated with a tag.
	 */
	public void startSubscription(ProcessBlock block,BlockProperty property) {
		tagListener.defineSubscription(block, property);
	}
	/**
	 * Stop the tag subscription associated with a particular property of a block.
	 */
	public void stopSubscription(ProcessBlock block,BlockProperty property) {
		if( property!=null && property.getValue()!=null && property.getBindingType()==BindingType.TAG ) {
			String tagPath = property.getValue().toString();
			if( tagPath!=null && tagPath.length()>0) {
				tagListener.stopSubscription(tagPath);
			}
		}
	}
	// ======================= Delegated to TagWriter ======================
	/**
	 * Write a value to a tag.
	 */
	public void updateTag(String providerName,String path,QualifiedValue val) {
		tagWriter.updateTag(providerName,path,val);
	}
	// ======================= Delegated to Watchdog ======================
	/**
	 * "pet" a watch dog. The watch dog must be updated to expire some time 
	 * in the future. This method may also be used to insert a watch dog
	 * into the timer list for the first time.
	 */
	public void pet(Watchdog dog) {
		watchdogTimer.updateWatchdog(dog);
	}
	/**
	 * Remove a watch dog. Delete it from the list.
	 */
	public void removeWatchdog(Watchdog dog) {
		watchdogTimer.removeWatchdog(dog);
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
					log.infof("%s.run: processing incoming note from buffer: %s:%s", TAG,inNote.getBlock().getBlockId().toString(),inNote.getPort());
					// Query the diagram to find out what's next
					ProcessBlock pb = inNote.getBlock();
					ProcessDiagram dm = modelManager.getDiagram(pb.getParentId());
					if( dm!=null) {
						Collection<IncomingNotification> outgoing = dm.getOutgoingNotifications(inNote);
						if( outgoing.isEmpty() ) log.warnf("%s: no downstream connections found ...",TAG);
						for(IncomingNotification outNote:outgoing) {
							UUID outBlockId = outNote.getConnection().getTarget();
							ProcessBlock outBlock = dm.getBlock(outBlockId);
							if( outBlock!=null ) {
								log.infof("%s.run: sending outgoing notification: to %s:%s", TAG,outNote.getConnection().getTarget().toString(),outNote.getConnection().getDownstreamPortName());
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
					log.infof("%s.run: processing broadcast request from buffer: %s = %s", TAG,inNote.getDiagramId().toString(),inNote.getSignal().getCommand());
					// Query the diagram to find out what's next. The diagramId is the resourceId
					ProcessDiagram dm = modelManager.getDiagram(inNote.getDiagramId());
					if( dm!=null) {
						Collection<SignalNotification> outgoing = dm.getBroadcastNotifications(inNote);
						if( outgoing.isEmpty() ) log.warnf("%s: no broadcast recipients found ...",TAG);
						for(SignalNotification outNote:outgoing) {
							ProcessBlock outBlock = outNote.getBlock();
							log.infof("%s.run: sending outgoing broadcast: to %s", TAG,outBlock.toString());
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


}
