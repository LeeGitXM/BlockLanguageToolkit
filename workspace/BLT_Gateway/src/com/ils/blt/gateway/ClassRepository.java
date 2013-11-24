/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.util.Hashtable;

import com.ils.block.BasicBlock;

/**
 * The class repository is a Singleton instance designed as a persistent common
 * repository for instances of objects that correspond to blocks in a diagram.
 * 
 * The repository is unique to each Designer instance. It's lifetime coincides with the
 * life of the Designer.
 */
public class ClassRepository extends Hashtable<String,BasicBlock> {
	private static final long serialVersionUID = 6666912014384473559L;
	private static ClassRepository instance = null;
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ClassRepository getInstance() {
		if( instance==null) {
			synchronized(ClassRepository.class) {
				instance = new ClassRepository();
			}
		}
		return instance;
	}
	/**
	 * Constructor is private per the Singleton pattern.
	 */
	private ClassRepository() {
	}
}
