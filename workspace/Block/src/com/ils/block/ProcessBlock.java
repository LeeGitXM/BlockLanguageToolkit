/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.block.TruthValue;
import com.ils.common.watchdog.WatchdogTimer;


/**
 * This interface defines an executable block in a diagram.
 * Each block carries its unique identity consisting of a projectId,
 * a diagramId and blockId - or alternatively a diagramId and blockId.
 * 
 * NOTE: auxiliaryData are not referenced here as, even though the
 *      information is serialized, there is no use of it in the Gateway.
 */
public interface ProcessBlock extends CoreBlock {

	/**
	 * If true, the "engine" will delay calling the start() method
	 * of this block until all other blocks that do not indicate
	 * a delay have been started. 
	 * @return true if this block should be ordered at the end
	 *         of the startup process. 
	 */
	public boolean delayBlockStart();
	/**
	 * Place a value on a named output port of a block. 
	 * This action does not change the internal state of the block.
	 * It's intended use is to debug a diagram.
	 * @param port the port on which to insert the specified value
	 * @param value a new value to be propagated along an
	 *        output connection. The string value will be coerced
	 *        into a data type appropriate to the connection.
	 */
	public void forcePost(String port, String value);
	
	/**
	 * @return the current state of the block
	 */
	public TruthValue getState();
	/**
	 * @return a string describing the status of the block. This 
	 * 		string is used for the dynamic block display.
	 */
	public String getStatusText();
	/**
	 * @return true if this block is locked for debugging purposes.
	 */
	public boolean isLocked();
	/**
	 * @return true if this block is a candidate for signal messages.
	 */
	public boolean isReceiver();
	/**
	 * @return true if this block publishes signal messages.
	 */
	public boolean isTransmitter();

	/**
	 * Set or clear the locked state of a block.
	 * @param flag True to lock the block.
	 */
	public void setLocked(boolean flag);

	/**
	 * Set the current state of the block.
	 * @param state
	 */
	public void setState(TruthValue state);
	/**
	 * @param text the current status of the block
	 */
	public void setStatusText(String text);
	/**
	 * Specify the timer to be used for all block-
	 * internal timings. 
	 * 
	 * @param timer
	 */
	public void setTimer(WatchdogTimer timer);
}