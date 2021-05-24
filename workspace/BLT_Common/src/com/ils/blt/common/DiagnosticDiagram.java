/**
 *   (c) 2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

/**
 * A diagram is the parent container for related blocks. Currently this interface
 * holds only a portion of the methods available on the concrete object (a ProcessDiagram).
 */
public interface DiagnosticDiagram  {
	/**
	 * @param root the subject block
	 * @return a list of blocks connected directly to the output(s) of the specified block.
	 */
	public List<ProcessBlock> getDownstreamBlocks(ProcessBlock root);
	/**
	 * List all blocks on the diagram.
	 * @return a collection of all blocks on the diagram;
	 */
	public Collection<ProcessBlock> getProcessBlocks();
	
	public UUID getSelf();
	public DiagramState getState();
	
	/**
	 * @param root the subject block
	 * @return a list of blocks connected directly to the input(s) of the specified block.
	 */
	public List<ProcessBlock> getUpstreamBlocks(ProcessBlock root);
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the input(s) of the specified block.
	 *         If the block is a "Source", then the list includes blocks
	 *         attached to all its linked "Sinks"
	 */
	public List<ProcessBlock> getUpstreamBlocksCrossingConnections(ProcessBlock root);
	
}