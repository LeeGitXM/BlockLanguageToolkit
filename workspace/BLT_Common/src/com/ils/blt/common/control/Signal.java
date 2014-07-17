/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

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
	private String message = "";
	private String pattern = null;
	
	/**
	 * Constructor: No-arg variety required to be serializable
	 */
	public Signal() {
		
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param command
	 * @param arg
	 * @param msg
	 */
	public Signal(String command,String arg,String msg)  {	
		this.command = command;
		this.arg = arg;
		this.message = msg;
	}


	public String getCommand() {return command;}
	public String getArg() {return arg;}
	public String getMessage() {return message;}
	public String getPattern() {return pattern;}
	public void setCommand(String cmd) {this.command = cmd;}
	public void setArg(String a) {this.arg = a;}
	public void setMessage(String msg) {this.message = msg;}
	public void setPattern(String pattern) {this.pattern = pattern;}

	@Override
	public String toString() {
		return String.format("%s:%s:%s",command,arg,message);
	}
}
