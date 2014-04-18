/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.test.gateway.mock;

import java.util.UUID;

import com.ils.block.AbstractProcessBlock;
import com.ils.block.ProcessBlock;


/**
 * This block is strictly for use with a MockDiagram to provide
 * inputs that can be subscribed to tags.
 */
public class MockInputBlock extends AbstractProcessBlock implements ProcessBlock {
	
	
	public MockInputBlock(UUID parent,String tagPath,PropertyType pt,String port) {
		super(BlockController.getInstance(),parent,UUID.randomUUID());
	}
	

}
