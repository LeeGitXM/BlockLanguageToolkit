/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.diagnostics.client;

import java.util.concurrent.ConcurrentHashMap;

import javax.swing.JFrame;

import org.apache.log4j.Level;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  The frame repository a central keeper of the JFrame object for each client.
 *  The frame is the component that surrounds the internal Java components. It
 *  is a common parent.
 *  
 *  We key the frame by user session. This is probably overkill, as the there
 *  should be only one JVM per project.Oh well ...
 */
public class FrameRepository   {
	//private static final String TAG = "FrameRepository: ";
	private static FrameRepository instance = null;
	private final LoggerEx log;
	private final ConcurrentHashMap<String,JFrame> frameMap;
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static FrameRepository getInstance() {
		if( instance==null) {
			synchronized(FrameRepository.class) {
				instance = new FrameRepository();
			}
		}
		return instance;
	}
	/**
	 * Constructor is private per the Singleton pattern.
	 */
	private FrameRepository() {;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		frameMap = new ConcurrentHashMap<String,JFrame>() ;
	}

	/**
	 * Correlate a frame object to the session.
	 * @param sessionId the current client session
	 * @param frame the enclosing JFrame
	 */
	public void setFrame(String sessionId,JFrame frame) {
		frameMap.put(sessionId, frame);
	}
	
	
	/**
	 * @param sessionId the current client session
	 * @return a frame previously cached for this session.
	 */
	public JFrame getFrame(String sessionId) {
		return frameMap.get(sessionId);
	}
	
}
