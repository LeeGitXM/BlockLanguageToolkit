package com.ils.blt.common;

import com.ils.blt.common.block.BlockConstants;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;

/**
 * This class presents several static utility methods dealing with
 * validation of business rules.
 */
public class BusinessRules
{
	/**
	 * The connections folder is the location for Sources/Sinks bound tags
	 * and NOT the location of bound tags for other uses.
	 * @param path tag path
	 * @return true if path is contained within the standard.
	 */
	public static boolean isStandardConnectionsFolder(TagPath tp) {
		String path = tp.toStringFull();
		return isStandardConnectionsFolder(path);
	}
	/**
	 * The connections folder is the location for Sources/Sinks bound tags
	 * and NOT the location of bound tags for other uses.
	 * @param path as a string
	 * @return true if path is contained within the standard.
	 */
	public static boolean isStandardConnectionsFolder(String path) {
		// Remove provider, if any
		int pos = path.indexOf("]");
		if(pos>0) {
			path = path.substring(pos+1);
		}
		return path.startsWith(BlockConstants.SOURCE_SINK_TAG_FOLDER);
	}

}
