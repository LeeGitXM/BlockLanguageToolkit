/**
 *   (c) 2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common;

import java.util.List;

/**
 * A diagram is the parent container for related blocks. Currently this interface
 * holds only a portion of the methods available on the concrete object (a ProcessDiagram).
 */
public interface DiagnosticDiagram  {
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the output(s) of the specified block.
	 */
	public List<ProcessBlock> getDownstreamBlocks(ProcessBlock root);
	
	/**
	 * @param root the subject block
	 * @return a list of blocks connected to the input(s) of the specified block.
	 */
	public List<ProcessBlock> getUpstreamBlocks(ProcessBlock root);
}