/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

public class Migrator {
	private static final String USAGE = "Usage: migrator <database>";
	
	public Migrator() {

	}
	
	
	/**
	 * Entry point for the application. 
	 * Usage: Migrator <databasepath> 
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args) {
		System.out.println(USAGE);
	}

}
