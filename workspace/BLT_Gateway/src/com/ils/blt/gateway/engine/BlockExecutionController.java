/**
 *   (c) 2012  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.gateway.engine;

import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.ExecutionController;
import com.ils.block.control.ValueChangeNotification;
import com.ils.common.BoundedBuffer;
import com.ils.common.watchdog.WatchdogTimer;
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
	private static int BUFFER_SIZE = 100;   // Buffer Capacity
	private final LoggerEx log;
	private GatewayContext context = null;    // Must be initialized before anything works
	private final ModelResourceManager delegate;
	private final WatchdogTimer watchdogTimer;
	private static BlockExecutionController instance = null;


	private final BoundedBuffer buffer;
	private TagListener tagListener = null;    // Tag subscriber
	private Thread completionThread = null;
	private boolean stopped = true;
	private ExecutorService executor = Executors.newCachedThreadPool();
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockExecutionController() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		delegate = new ModelResourceManager(this);
		
		buffer = new BoundedBuffer(BUFFER_SIZE);
		watchdogTimer = new WatchdogTimer();
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
	 * A block has completed evaluation. A new value has been placed on its output.
	 */
	public void acceptCompletionNotification(ValueChangeNotification note) {
		try {
			buffer.put(note);
		}
		catch( InterruptedException ie ) {}
	}
	/**
	 * Set the gateway context. Once the context is known, we can create a dataCollector.
	 * @param ctxt the context
	 */
	public void setContext(GatewayContext ctxt) { 
		this.context=ctxt; 
		this.tagListener = new TagListener(context);
		this.delegate.setContext(context);
	}

	public synchronized void start() {
		stopped = false;
		completionThread = new Thread(this, "BlockCompletionHandler");
		log.debugf("%s START %d",TAG,completionThread.hashCode());
		completionThread.setDaemon(true);
		completionThread.start();
	}
	
	public synchronized void stop() {
		log.debug(TAG+"STOPPED");
		stopped = true;
		executor.shutdown();
		if(completionThread!=null) {
			completionThread.interrupt();
		}
		tagListener.stop();
	}
	
	public ModelResourceManager getDelegate() { return delegate; }


	

	
	// ======================= Delegated to TagListener ======================
	/**
	 * Start a subscription for a block attribute associated with a tag.
	 */
	public void startSubscription(ProcessBlock block,BlockProperty property) {
		tagListener.startSubscription(block, property);
	}
	/**
	 * Stop the subscription for a block attribute associated with a tag.
	 */
	public void stopSubscription(ProcessBlock block,String propertyName) {
		BlockProperty property = block.getProperty(propertyName);
		if( property!=null ) {
			String tagPath = property.getValue().toString();
			if( tagPath!=null) {
				tagListener.stopSubscription(tagPath);
			}
		}
	}
	
	// ============================ Completion Handler =========================
	/**
	 * Wait for work to arrive at the output of a bounded buffer. The contents of the bounded buffer
	 * are ExecutionCompletionNotification objects.
	 */
	public void run() {
		while( !stopped  ) {
			try {
				Object work = buffer.get();
				if( work instanceof ValueChangeNotification) {
					ValueChangeNotification inNote = (ValueChangeNotification)work;
					// Query the diagram to find out what's next
					ProcessBlock pb = inNote.getBlock();
					ProcessDiagram dm = delegate.getDiagram(new Long(pb.getProjectId()),new Long(pb.getDiagramId()));
					if( dm!=null) {
						Collection<ValueChangeNotification> outgoing = dm.getOutgoingNotifications(inNote);
						for(ValueChangeNotification outNote:outgoing) {
							executor.execute(new IncomingValueChangeTask(outNote));
						}
					}
					else {
						log.warnf("%s: run: diagram %d in project %d in value change notification not found",TAG,
									pb.getDiagramId(),pb.getProjectId());
					}
				}
				else {
					log.warnf("%s: run: Unexpected object in buffer (%s)",TAG,work.getClass().getName());
				}
			}
			catch( InterruptedException ie) {}
		}
	}
}
