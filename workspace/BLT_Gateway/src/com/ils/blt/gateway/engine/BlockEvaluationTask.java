/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.block.BasicBlock;

/**
 * Evaluate the specified block. The thread should end with the block placing a
 * value on its output.
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
