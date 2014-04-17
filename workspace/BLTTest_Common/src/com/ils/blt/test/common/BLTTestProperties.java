/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.common;

/**
 *  Define properties that are common to all scopes.
 */
public interface BLTTestProperties   {
	public final static String MODULE_ID = "blocktest";        // See module-blt-test.xml
	public final static String MODULE_NAME = "BLTTest";        // See build-blt-test.xml
	public final static String MOCK_SCRIPT_PACKAGE = "system.ils.test.mock";        // Python package name for block test
	public final static String TIMESTAMP_FORMAT = "yyyy.MM.dd HH:mm:ss.SSS";        // Format for writing timestamp
}
