/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BlockPropertyChangeListener;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;


/**
 * This interface defines an executable block in a diagram.
 * Each block carries its unique identity consisting of a projectId,
 * a diagramId and blockId - or alternatively a diagramId and blockId.
 * 
 * NOTE: auxiliaryData are not referenced here as, even though the
 *      information is serialized, there is no use of it in the Gateway.
 */
public interface ProcessBlock extends BlockPropertyChangeListener {
	/**
	 * Notify the block that a new value has appeared on one of its
	 * input anchors. The notification contains the upstream source
	 * block, the port and value.
	 * @param incoming notification 
	 */
	public void acceptValue(IncomingNotification incoming);
	/**
	 * Notify the block that it is the recipient of a signal from
	 * "the ether". This signal is not associated with a connection.
	 * This method is meaningful only for blocks that are "receivers".
	 * @param sn incoming signal 
	 */
	public void acceptValue(SignalNotification sn);
	/**
	 * If true, the "engine" will delay calling the start() method
	 * of this block until all other blocks that do not indicate
	 * a delay have been started. 
	 * @return true if this block should be ordered at the end
	 *         of the startup process. 
	 */
	public boolean delayBlockStart();
	/**
	 * Calculate the block state. This may or may not result in any output.
	 * 
	 * In the case where the block has specified a coalescing time,
	 * this method will be called by the engine after receipt of input
	 * once the coalescing "quiet" time has passed without further input.
	 */
	public void evaluate();
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
	 * @return a list of anchor prototypes for the block.
	 */
	public List<AnchorPrototype> getAnchors();

	/**
	 * @return the universally unique Id of the block.
	 */
	public UUID getBlockId();
	/**
	 * @return information necessary to populate the block 
	 *          palette and subsequently paint a new block
	 *          dropped on the workspace.
	 */
	public PalettePrototype getBlockPrototype();
	/**
	 * @return the fully qualified path name of this block.
	 */
	public String getClassName();
	/**
	 * Blocks that have a logical output are responsible for 
	 * compiling a string that describes the reason for either 
	 * a TRUE or FALSE result. If the block has logical inputs
	 * then the explanation accounts for upstream explanations.
	 * 
	 * @param diagram the diagram on which this block is placed.
	 * @param members blocks that have been visited.
	 * @return an explanation for the current state of the block.
	 *         If this is a block that has no relevant state, return
	 *         an empty string.
	 */
	public String getExplanation(DiagnosticDiagram diagram,List<UUID> members);
	/**
	 * @return information related to the workings of the block.
	 *        The information returned varies depending on the 
	 *        block. At the very least the data contains the 
	 *        block UUID and class. The data is read-only.
	 */
	public SerializableBlockStateDescriptor getInternalStatus();
	/**
	 * @return the block's most recent internal value
	 */
	public QualifiedValue getLastValue();
	/**
	 * @return the block's label
	 */
	public String getName();
	
	/**
	 * @return the Id of the block's diagram (parent).
	 */
	public UUID getParentId();
	/**
	 * @return the id of the project under which this block was created.
	 */
	public long getProjectId() ;
	/**
	 * @return all properties of the block. The array may be used
	 * 			to updated properties directly.
	 */
	public BlockProperty[] getProperties();
	/**
	 * @param name property name
	 * @return a particular property by name.
	 */
	public BlockProperty getProperty(String name);
	/**
	 * @return a list of names of properties known to this class.
	 */
	public Set<String> getPropertyNames() ;
	
	/**
	 * @return the current state of the block
	 */
	public TruthValue getState();
	/**
	 * @return the current state of the block
	 */
	public Date getTimeOfLastStateChange();
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
	 * Send status update notifications for any properties
	 * or output connections known to the designer. 
	 * 
	 * In practice, the block properties are all updated
	 * when a diagram is opened. It's the connection
	 * notification for animation that is most necessary.
	 */
	public void notifyOfStatus();
	/**
	 * Artificially place the current block state on its
	 * outputs. This method is never called in the normal
	 * course of block operation.
	 */
	public void propagate();
	/**
	 * Add a time-stamped entry to the block's activity log.
	 * The log is viewable as part of the internal status.
	 * @param desc description of the activity being recorded. 
	 *        Presumably this comes from a controlled vocabulary
	 * @param value a new value associated with the activity, if any.
	 */
	public void recordActivity(String desc,String value);
	public void recordActivity(String desc,String name,String value);
	/**
	 * Reset the internal state of the block.
	 */
	public void reset();
	/**
	 * Set the anchor descriptors.
	 * @param prototypes descriptions of anchors
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
	 * @param state new block state
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
	 * @param timer watchdog timer for production mode
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
	 * Convert the block into a portable, serializable description.
	 * The descriptor holds common attributes of the block.
	 * @return the descriptor
	 */
	public SerializableBlockStateDescriptor toDescriptor();
	
	/**
	 * @param tagpath of the tag to be considered
	 * @return true if any property of the block is bound to
	 *         the supplied tagpath. The comparison does not
	 *         consider the provider portion of the path.
	 */
	public boolean usesTag(String tagpath);
	/**
	 * Check the block configuration for missing or conflicting
	 * information.
	 * @return a validation summary. Null if everything checks out.
	 */
	public String validate();
	/**
	 * Check any properties that are bound to tags. Verify that the
	 * property matches the current value of the tag.
	 * @return a validation summary. Null if everything checks out.
	 */
	public String validateSubscription();
	
	//===================== PropertyChangeListener ======================
	/**
	 * This is a stricter implementation that enforces QualifiedValue data.
	 */
	public void propertyChange(BlockPropertyChangeEvent event);
}