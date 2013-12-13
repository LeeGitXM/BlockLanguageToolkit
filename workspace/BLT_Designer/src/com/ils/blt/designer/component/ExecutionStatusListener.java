/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.sct.designer;

import java.util.EventListener;

public interface ExecutionStatusListener extends EventListener {
	void executionComplete(ExecutionStatusEvent event);
}
