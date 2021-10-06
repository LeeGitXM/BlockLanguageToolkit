/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.util.LogUtil;

/**
 * A Source Connection is a special class that receives values directly
 * from a tag that is meant to be logically connected to a SinkConnection. 
 * Connected sources and sinks should share common names.
 */
@ExecutableBlock
public class SourceConnection extends Input implements ProcessBlock {
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SourceConnection() {
		initialize();
		initializePrototype();
		log = LogUtil.getLogger(getClass().getPackage().getName()+".source");
		log.debugf("Creating a SourceConnection for the palette");
	}

	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public SourceConnection(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
	}
	
	/**
	 * Make the tag path property non-editable
	 */
	@Override
	protected void initialize() {
		super.initialize();
		setName("SourceConnection");
		tagPathProperty.setEditable(false);
	}
	
	/**
	 * Guarantee that the clas name matches the constant used throughout
	 * the application to identify a source.
	 */
	@Override
	public String getClassName() { return BlockConstants.BLOCK_CLASS_SOURCE; }
	
	
	/**
	 * A source block has has a special form of the explanation method in that
	 * the explanation is derived from the most recent block to write to its
	 * input. We have no direct way of knowing which of the potential inputs 
	 * wrote to the tag. Consequently, we search the potential sources and
	 * choose the one that has the latest timestamp. 
	 * 
	 * @return an explanation for the current state of the block.
	 *         By default it is the concatenated explanations of all 
	 *         upstream blocks with the same state.
	 *         If this is a block that has no relevant state, return
	 *         an empty string.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram diagram,List<UUID> members) {
		String explanation = "";
		members.add(getBlockId());
		TruthValue blockState = getState();
		if( blockState.equals(TruthValue.TRUE) || blockState.equals(TruthValue.FALSE)) {
			ProcessBlock upstream = getMostRecentlyChangedPredecessor(diagram);
			if( upstream!=null ) {
				if(members.contains(upstream.getBlockId())) {
					explanation = explanation + "-- truncated (circular reasoning)";
				}
				else {
					DiagnosticDiagram connectedDiagram = controller.getDiagram(upstream.getParentId().toString());
					if(!explanation.isEmpty()) explanation = explanation + ", ";
					explanation = explanation + upstream.getExplanation(connectedDiagram,members);
				}
			}
		}
		return explanation;
	}
	
	/**
	 * Search the upstream blocks to find the one most recently changed
	 * and that matches the current state of this block.
	 * @return
	 */
	private ProcessBlock getMostRecentlyChangedPredecessor(DiagnosticDiagram diagram) {
		ProcessBlock result = null;
		Date latestTime = null;
		List<SerializableBlockStateDescriptor>predecessors = controller.listSinksForSource(diagram.getSelf().toString(), getName());
		for( SerializableBlockStateDescriptor predecessor:predecessors ) {
			String connectedDiagramId = predecessor.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_PARENT);
			String connectedBlockId = predecessor.getIdString();
			ProcessBlock block = controller.getProcessBlock(connectedDiagramId,connectedBlockId);
			if( getState().equals(block.getState())) {
				if( latestTime==null ||
					(block.getTimeOfLastStateChange()!=null &&
					 latestTime.getTime() < block.getTimeOfLastStateChange().getTime())       ) {
					result = block;
					latestTime = block.getTimeOfLastStateChange();
				}
			}
		}
		return result;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/source.png");
		prototype.setPaletteLabel("Source");
		prototype.setTooltipText("Receive data from a sink of the same name");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(50);
		desc.setBackground(new Color(127,127,127).getRGB()); // Dark gray
		desc.setCtypeEditable(true);
	}
	
	/**
	 * In addition to the standard validation, make sure that there is
	 * something from which we can receive an input.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String summary = super.validate();
		if( summary==null ) {
			List<SerializableBlockStateDescriptor> links = controller.listSinksForSource(getParentId().toString(),getName());
			if( links.isEmpty() ) {
				summary = String.format("There are no sinks linked to this source block\t");
			}
		}
		
		return summary;
	}
}