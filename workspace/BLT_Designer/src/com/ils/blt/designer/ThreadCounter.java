/**
 *   (c) 2016  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.designer;

/**
 *  The block execution controller is responsible for the dynamic activity for the collection
 *  of diagrams. It receives status updates from the RPC controller and from the resource manager
 *  which is its delegate regarding model changes. The changes are analyzed to
 *  determine if one or more downstream blocks are to be informed of the change.
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class ThreadCounter  {
	private static ThreadCounter instance = null;
	private int count = 0;

	/**
	 * Per the Singleton pattern, the constructor is private.
	 */
	private ThreadCounter() {
		count = 0;
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ThreadCounter getInstance() {
		if( instance==null) {
			synchronized(ThreadCounter.class) {
				instance = new ThreadCounter();
			}
		}
		return instance;
	}
	
	public boolean isQuiescent() { return count==0; }
	public void incrementCount() { count++; }
	public void decrementCount() { count--; }
	public void reset() { count=0; }
}
