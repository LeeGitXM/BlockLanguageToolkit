/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.List;

import org.python.core.PyObject;
import com.ils.block.common.PalettePrototype;

/**
 *  This class exposes python-callable functions that deal with block properties.
 *  We make use of the BlockPropertiesRequestHandler to perform the requests.
 */
public class BlockPropertiesScriptFunctions   {

	/**
	 * Query the Python block project for a list of PalettePrototypes that 
	 * correspond to the blocks implemented in Python. The availability of 
	 * this method through the scripting interface is primarily for debugging.
	 * 
	 */
	public static void getBlockPrototypes() {
		BlockPropertiesRequestHandler handler = new BlockPropertiesRequestHandler();
		List<PalettePrototype> prototypes = handler.getBlockPrototypes();
	}
}