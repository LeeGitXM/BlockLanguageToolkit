/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.test.client;

import com.ils.blt.test.common.MockDiagramScriptingInterface;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes the methods available to a designer/client for the
 *  purposes of testing BLT blocks.
 */
public class MockDiagramScriptFunctions implements MockDiagramScriptingInterface  {
	private static final String TAG = "MockDiagramScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(MockDiagramScriptFunctions.class.getPackage().getName());
	
}