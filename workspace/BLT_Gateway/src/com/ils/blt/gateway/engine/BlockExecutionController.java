/**
 *   (c) 2012  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.gateway.engine;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.ExecutionController;
import com.ils.block.control.NewValueNotification;
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
	private DataCollector dataCollector = null;    // Tag subscriber
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
	
	public void acceptCompletionNotification(NewValueNotification note) {
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
		this.dataCollector = new DataCollector(context);
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
		dataCollector.stop();
	}
	
	public ModelResourceManager getDelegate() { return delegate; }


	

	
	// ======================= Delegated to DataCollector ======================
	/**
	 * Start a subscription for a block attribute associated with a tag.
	 */
	public void startSubscription(ProcessBlock block,String propertyName) {
		dataCollector.startSubscription(block, propertyName);
	}
	/**
	 * Stop the subscription for a block attribute associated with a tag.
	 */
	public void stopSubscription(ProcessBlock block,String propertyName) {
		BlockProperty property = block.getProperty(propertyName);
		if( property!=null ) {
			String tagPath = property.getValue().toString();
			if( tagPath!=null) {
				dataCollector.stopSubscription(tagPath);
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
				if( work instanceof NewValueNotification) {
					NewValueNotification incoming = (NewValueNotification)work;
					// Query the diagram to find out what's next
					ProcessBlock pb = incoming.getBlock();
					ProcessDiagram dm = delegate.getDiagram(new Long(pb.getProjectId()),new Long(pb.getDiagramId()));
					if( dm!=null) {
						NewValueNotification outgoing = dm.getValueUpdate(incoming);
						if( outgoing!=null ) {
							executor.execute(new PortReceivesValueTask(outgoing));
						}

						else {
							log.warnf("%s: run: diagram %d in project %d in execution complete notification not found",TAG,
									pb.getDiagramId(),pb.getProjectId());
						}
					}
					else {
						log.warnf("%s: run: project %d in execution complete notification not found",TAG,pb.getProjectId());
					}
				}
			}
			catch( InterruptedException ie) {}
		}
	}
}
