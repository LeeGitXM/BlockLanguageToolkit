/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * We've had trouble using a ProjectResourceId as a key for a HashMap. This class
 * creates a unique string from a ProjectResourceId.
 */
public class ResourceKey
{
	/**
	 * @param resid resourceId corresponding to the resource
	 * @return  a text key for for selection of a diagram. 
	 */
	public static String keyForResource(ProjectResourceId resid) {
		return String.format("%s:%s",resid.getProjectName(),resid.getResourcePath().getPath().toString());
	}
}
