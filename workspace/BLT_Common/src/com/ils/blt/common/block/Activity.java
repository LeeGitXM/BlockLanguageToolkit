package com.ils.blt.common.block;

import java.io.Serializable;
import java.util.Date;


/**
 * An activity holds an "incident" in the history of a block.
 */
public class Activity implements Cloneable,Serializable  {
	private static final long serialVersionUID = -6862986291141763186L;
	// These are some standard actions
	public final static String ACTIVITY_BLOCKED = "BLOCKED";
	public final static String ACTIVITY_PROPERTY = "PROPERTY";
	public final static String ACTIVITY_RECEIVE  = "RECV";
	public final static String ACTIVITY_RESET = "RESET";
	public final static String ACTIVITY_SEND  = "SEND";
	public final static String ACTIVITY_SET_EXPIRATION = "SET EXPIRATION";
	public final static String ACTIVITY_STATE = "STATE";
	public final static String ACTIVITY_START = "START";
	public final static String ACTIVITY_STOP  = "STOP";
	private Date timestamp;
	private String action;   // Name of the activity (from a controlled vocabulary)
	private String value;
	
	public Activity() {
		this.timestamp = new Date();  // Now
		this.action = "";
		this.value = "";
	}
	public Activity(String act,String val) {
		this.timestamp = new Date();  // Now
		this.action = act;
		this.value = val;
	}

	public Date getTimestamp() {return timestamp;}
	public void setTimestamp(Date tstamp) { this.timestamp=tstamp;}
	public String getAction() {return action;}
	public void setAction(String act) { this.action=act;}
	public String getValue() {return value;}
	public void setValue(String val) { this.value=val;}
	
	@Override
	public Activity clone() {
		Activity a = new Activity(this.action,this.value);
		a.setTimestamp(this.timestamp);
		return a;
	}
	
}
