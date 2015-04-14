/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BlockPropertyChangeListener;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.WatchdogTimer;


/**
 * This interface defines an executable block in a diagram.
 * Each block carries its unique identity consisting of a projectId,
 * a diagramId and blockId. 
 */
public interface ProcessBlock extends BlockPropertyChangeListener {
	/**
	 * Notify the block that a new value has appeared on one of its
	 * input anchors. The notification contains the upstream source
	 * block, the port and value.
	 * @param vcn 
	 */
	public void acceptValue(IncomingNotification vcn);
	/**
	 * Notify the block that it is the recipient of a signal from
	 * "the ether". This signal is not associated with a connection.
	 * This method is meaningful only for blocks that are "receivers".
	 * @param sn 
	 */
	public void acceptValue(SignalNotification sn);
	/**
	 * @return a list of anchor prototypes for the block.
	 */
	public List<AnchorPrototype> getAnchors();
	/**
	 * @return the fully qualified path name of this block.
	 */
	public String getClassName();
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
	 * @return the Id of the block's diagram (parent).
	 */
	public UUID getParentId();
	/**
	 * @return the universally unique Id of the block.
	 */
	public UUID getBlockId();
	/**
	 * @return the block's label
	 */
	public String getName();
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
	 * @return information necessary to populate the block 
	 *          palette and subsequently paint a new block
	 *          dropped on the workspace.
	 */
	public PalettePrototype getBlockPrototype();
	/**
	 * @return information related to the workings of the block.
	 *        The information returned varies depending on the 
	 *        block. At the very least the data contains the 
	 *        block UUID and class. The data is read-only.
	 */
	public SerializableBlockStateDescriptor getInternalStatus();
	/**
	 * @return a particular property by name.
	 */
	public BlockProperty getProperty(String name);
	/**
	 * @return the id of the project under which this block was created.
	 */
	public long getProjectId() ;
	/**
	 * @return a list of names of properties known to this class.
	 */
	public Set<String> getPropertyNames() ;
	
	/**
	 * @return all properties of the block. The array may be used
	 * 			to updated properties directly.
	 */
	public BlockProperty[] getProperties();
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
	 * Send status update notifications for any properties
	 * or output connections known to the designer. 
	 * 
	 * In practice, the block properties are all updated
	 * when a diagram is opened. It's the connection
	 * notification for animation that is most necessary.
	 */
	public void notifyOfStatus();
	/**
	 * Reset the internal state of the block.
	 */
	public void reset();
	/**
	 * Set the anchor descriptors.
	 * @param prototypes
	 */
	public void setAnchors(List<AnchorPrototype> prototypes);
	/**
	 * Set or clear the locked state of a block.
	 * @param flag True to lock the block.
	 */
	public void setLocked(boolean flag);
	/**
	 * @param name the name of the block. The name
	 *        is guaranteed to be unique within a 
	 *        diagram.
	 */
	public void setName(String name);
	/**
	 * @param id is the project to which this block belongs.
	 */
	public void setProjectId(long id);
	/**
	 * Accept a new value for a block property. It is up to the
	 * block to determine whether or not this triggers block 
	 * evaluation.
	 * @param name of the property to update
	 * @param value new value of the property
	 */
	public void setProperty(String name,Object value);
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
	/**
	 * Start any active monitoring or processing within the block.
	 */
	public void start();
	/**
	 * Terminate any active operations within the block.
	 */
	public void stop();
	/**
	 * In the case where the block has specified a coalescing time,
	 * this method will be called by the engine after receipt of input
	 * once the coalescing "quiet" time has passed without further input.
	 */
	public void evaluate();
	//===================== PropertyChangeListener ======================
	/**
	 * This is a stricter implementation that enforces QualifiedValue data.
	 */
	public void propertyChange(BlockPropertyChangeEvent event);
}