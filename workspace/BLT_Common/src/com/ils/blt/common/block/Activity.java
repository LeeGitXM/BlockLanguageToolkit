package com.ils.blt.common.block;

import java.util.Date;


/**
 * An activity holds an "incident" in the history of a block.
 */
public class Activity  {
	// These are some standard actions
	public final static String ACTIVITY_RESET = "RESET";
	public final static String ACTIVITY_STATE = "STATE";
	private final Date timestamp;
	private final String action;   // Name of the activity (from a controlled vocabulary)
	private final String value;
	
	public Activity(Date tstamp,String act,String val) {
		this.timestamp = tstamp;
		this.action = act;
		this.value = val;
	}

	public Date getTimestamp() {return timestamp;}

	public String getAction() {return action;}

	public String getValue() {return value;}
	
}
