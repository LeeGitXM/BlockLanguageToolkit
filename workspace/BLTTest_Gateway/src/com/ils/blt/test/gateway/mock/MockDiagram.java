/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.test.gateway.mock;

import java.util.UUID;

import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.gateway.engine.ProcessDiagram;

/**
 * A mock diagram is a process diagram, specially created for functional testing
 * of blocks. 
 */
public class MockDiagram extends ProcessDiagram {
	
	private static String TAG = "MockDiagram";
	
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the unserialized object that represents the diagram.
	 * @param parent 
	 */
	public MockDiagram(SerializableDiagram diagm,UUID parent) { 
		super(diagm,parent);
		
	}

}
