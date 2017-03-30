/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import java.io.Serializable;


/**
 * A signal is a command and is transmitted along a Signal connection
 * or is broadcast.
 * 
 * This is a property container with no behavior.
 */
public class Signal implements Serializable {
	private static final long serialVersionUID = 3694163288904551177L;
	private String command = "";
	private String arg = "";
	private String payload = "";
	private String pattern = "";
	
	/**
	 * Constructor: No-arg variety required to be serializable
	 */
	public Signal() {
		
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param command a name
	 * @param argument optional string for filtering
	 * @param payload optional string containing the contents of the message
	 */
	public Signal(String command,String argument,String payload)  {	
		this.command = command;
		this.arg = argument;
		this.payload = payload;
	}


	public String getCommand() {return command;}
	public String getArgument() {return arg;}
	public String getPayload() {return payload;}
	public String getPattern() {return pattern;}
	public void setCommand(String cmd) {this.command = cmd;}
	public void setArgument(String a) {this.arg = a;}
	public void setPayload(String msg) {this.payload = msg;}
	public void setPattern(String pat) {this.pattern = pat;}


	@Override
	public String toString() {
		return String.format("%s:%s:%s",command,arg,payload);
	}
}
