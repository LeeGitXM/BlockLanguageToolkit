/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.block.BasicBlock;

/**
 * Evaluate the specified block. The thread should end with the block placing a
 * value on its output.
 * 
 * This task exists in two flavors. Both flavors involve new data for the block
 * and then a call to its evaluate() method. 
 * 
 * In the first case, the block responds to an input that is mapped to a tag.
 * The tag is not "connected" to another block.
 * 
 * In the second instance, a value has been received as an output from a block
 * connected to the target block.
 */
public class BlockEvaluationTask implements Runnable{
	private final BasicBlock block;
	/**
	 * Constructor.
	 * 
	 * @param bblock the block instance to evaluate
	 */
	public BlockEvaluationTask(BasicBlock bblock)  {	
		this.block = bblock;
	}
	
	public void run()   { 
		block.evaluate();
	}
}
