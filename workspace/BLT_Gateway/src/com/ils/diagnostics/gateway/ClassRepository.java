/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.diagnostics.gateway;

import org.python.core.PyDictionary;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * The class repository is a Singleton instance designed as a persistent common
 * repository for Python instances of objects that correspond to blocks in a diagram.
 * We never actually look at the objects from the Java side.
 * 
 * The repository is unique to each Designer instance. It's lifetime coincides with the
 * life of the Designer. We expose this directly to python.
 */
public class ClassRepository {
	private static final String TAG = "ClassRepository:";
	private static ClassRepository instance = null;
	private final PyDictionary repository;

	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
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
		repository = new PyDictionary();
	}
	/**
	 * Get the repository. 
	 * 
	 * @return the object repository.
	 */
	public PyDictionary getRepository() {
		return repository;
	}
}
