/**
 *   (c) 2012  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.gateway.engine;

import java.util.Hashtable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.common.BoundedBuffer;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;



/**
 *  The block execution controller receives status updates from the RPC controller
 *  and from the resource manager regarding model changes. The changes are analyzed to
 *  determine if one or more downstream blocks are to be informed of the change.
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class BlockExecutionController implements Runnable {
	private final static String TAG = "BlockExecutionController";
	private static int BUFFER_SIZE = 100;   // Buffer Capacity
	private static int POOL_SIZE = 4;       // Number of threads for block execution
	private final LoggerEx log;
	private GatewayContext context = null;    // Must be initialized before anything works
	private static BlockExecutionController instance = null;
	/** The diagrams are keyed by projectId, then resourceID */
	private final Hashtable<Long,Hashtable<Long,DiagramModel>> models;
	private final BoundedBuffer buffer;
	private DataCollector dataCollector = null;    // Tag subscriber
	private Thread completionThread = null;
	private boolean stopped = true;
	private ExecutorService executors = Executors.newCachedThreadPool();
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockExecutionController() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		models = new Hashtable<Long,Hashtable<Long,DiagramModel>>();
		buffer = new BoundedBuffer(BUFFER_SIZE);
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
	
	public void acceptCompletionNotification(ExecutionCompletionNotification note) {
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
		executors.shutdown();
		if(completionThread!=null) {
			completionThread.interrupt();
		}
	}
	
	/**
	 * Remove all block resources associated with a project.
	 * Presumably the project has been deleted.
	 * @param projectId the identity of a project.
	 */
	public void deleteResources(Long projectId) {
		models.remove(projectId);
	}

	/**
	 * Remove a diagram within a project.
	 * Presumably the diagram has been deleted.
	 * @param projectId the identity of a project.
	 */
	public void deleteResource(Long projectId,Long resourceId) {
		Hashtable<Long,DiagramModel> projectModel = models.get(projectId);
		if( projectModel!=null ) projectModel.remove(resourceId);
	}
	
	/**
	 * Set a model for a diagram. There is a one-one correspondence 
	 * between a model-project and diagram.
	 * @param projectId the identity of a project
	 * @param resourceId the identity of the model resource
	 * @param model the diagram logic
	 */
	public void addResource(Long projectId,Long resourceId,DiagramModel model) {
		Hashtable<Long,DiagramModel> projectModels = models.get(projectId);
		if( projectModels==null ) {
			projectModels = new Hashtable<Long,DiagramModel>();
			models.put(projectId,projectModels);
		}
		projectModels.put(resourceId, model);
	}


	// ================================ Completion Handler =========================
	/**
	 * Wait for work to arrive at the output of a bounded buffer.
	 */
	public void run() {
		while( !stopped  ) {
			try {
				Object work = buffer.get();
				if( work instanceof ExecutionCompletionNotification) {
					ExecutionCompletionNotification ecn = (ExecutionCompletionNotification)work;
				}
			}
			catch( InterruptedException ie) {}
		}
	}
}
