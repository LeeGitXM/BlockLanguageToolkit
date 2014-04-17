/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.gateway;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes python-callable functions used to report block status
 *  changes to the Gateway. The functions are designed for access from 
 *  python implementation of blocks..
 *  
 *  Since we are in Gateway, we can make local calls.
 */
public class MockDiagramScriptFunctions  {
	private static final String TAG = "MockDiagramScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(MockDiagramScriptFunctions.class.getPackage().getName());
	
}