/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.test.gateway.mock;

import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.AnchorPrototype;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.connection.ConnectionType;
import com.ils.connection.ProcessConnection;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * A mock diagram is a process diagram, specially created for functional testing
 * of blocks. 
 */
public class MockDiagram extends ProcessDiagram {
	private static String TAG = "MockDiagram";
	private final LoggerEx log;
	ProcessBlock uut = null;    // Unit under test
	
	
	/**
	 * Constructor: Create a model that encapsulates the structure of the blocks and connections
	 *              of a diagram.
	 * @param diagm the unserialized object that represents the diagram.
	 * @param parent 
	 */
	public MockDiagram(SerializableDiagram diagm,UUID parent) { 
		super(diagm,parent);
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	public void addBlock(ProcessBlock block) {
		if( !(block instanceof MockInputBlock || block instanceof MockOutputBlock) ) uut = block;
		this.getProcessBlocks().add(block);
	}
	/**
	 * Compute connections based on the collection of input/output blocks.
	 *  Create connections between these and the unit-under-test.
	 *
	 */
	public void analyze() {
		if(uut!=null) {
			for(ProcessBlock block:this.getProcessBlocks()) {
				if( block instanceof MockInputBlock ) {
					MockInputBlock mib = (MockInputBlock)block;
					AnchorPrototype anchor = getAnchorForPort(mib.getPort());
					if( anchor!=null ) {
						ProcessConnection pc = 
								new ProcessConnection(ConnectionType.connectionTypeForPropertyType(mib.getPropertyType()));
						pc.setSource(mib.getBlockId());
						pc.setTarget(uut.getBlockId());
						pc.setDownstreamPortName(mib.getPort());
						pc.setUpstreamPortName(mib.getPort());
						this.addOutgoingConnection(pc);
					}
					else {
						log.warnf("%s.analyze: Block-under-test does not have an input port %s",TAG,mib.getPort());
					}
					
				}
				else if( block instanceof MockOutputBlock ) {
					
				}
			}
		}
	}

	public ProcessBlock getBlockUnderTest() { return uut; }
	
	/**
	 * Return the nth MockInputBlock connected to the named port. The
	 * connections are numbered in the order in which they were defined.
	 * @param port
	 * @param index of the connection to the named port. Zero-based.
	 * @return
	 */
	public MockInputBlock getInputForPort(String port,int index) {
		MockInputBlock result = null;
		int count = 0;
		for(ProcessBlock block:getProcessBlocks()) {
			if(block instanceof MockInputBlock) {
				MockInputBlock mib = (MockInputBlock)block;
				if(mib.getPort().equalsIgnoreCase(port)) {
					if( count==index) {
						result = mib;
						break;
					}
					count++;
				}
			}
		}
		return result;
	}
	
	public MockOutputBlock getOutputForPort(String port) {
		MockOutputBlock result = null;
		for(ProcessBlock block:getProcessBlocks()) {
			if(block instanceof MockOutputBlock) {
				MockOutputBlock mob = (MockOutputBlock)block;
				if(mob.getPort().equalsIgnoreCase(port)) {
					result = mob;
					break;
				}
			}
		}
		return result;
	}
	
	/**
	 * Return the nth MockInputBlock connected to the named port. The
	 * connections are numbered in the order in which they were defined.
	 * @param port
	 * @param index of the connection to the named port. Zero-based.
	 * @return
	 */
	private AnchorPrototype getAnchorForPort(String port) {
		AnchorPrototype anchor = null;
		if( uut!=null) {
			for(AnchorPrototype anc:uut.getBlockPrototype().getBlockDescriptor().getAnchors()) {
				if(anc.getName().equalsIgnoreCase(port)) {
					anchor = anc;
					break;
				}
			}
		}
		return anchor;
	}
}