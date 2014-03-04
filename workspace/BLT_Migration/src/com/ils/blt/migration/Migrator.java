/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class Migrator {
	private static final String USAGE = "Usage: migrator <database>";
	private final Map<String,String> classMap;   // Lookup by G2 classname
	
	public Migrator(Map<String,String>classLookup) {
		this.classMap = classLookup;
	}
	
	
	/**
	 * Entry point for the application. 
	 * Usage: Migrator <databasepath> 
	 * NOTE: For Windows, specify path as: C:/home/work/migrate.db
	 *       For Mac/Linux:    /home/work/migrate.db
	 * We automatically adjust windows path, if specified with backslashes.
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args) {
		
		
		// Look for databasen path as arg
		if( args.length == 0) {
			System.out.println(USAGE);
			System.exit(1);
		}
		String path = args[0];
		// In case we've been fed a Windows path, convert
		path = path.replace("\\", "/");
		String connectPath = "jdbc:sqlite:"+path;


		Connection connection = null;
		try
		{
			// create a database connection
			connection = DriverManager.getConnection(connectPath);
			Statement statement = connection.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			Map<String,String> classMap = new HashMap<String,String>();
			ResultSet rs = statement.executeQuery("select * from ClassMap");
			while(rs.next())
			{
				// read the result set
				String g2 = rs.getString("G2Class");
				String ignition = rs.getString("IgnitionClass");
				classMap.put(g2, ignition);
			}
			Migrator m = new Migrator(classMap);
		}
		catch(SQLException e)
		{
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(e.getMessage());
		}
		finally
		{
			try
			{
				if(connection != null)
					connection.close();
			}
			catch(SQLException e)
			{
				// connection close failed.
				System.err.println(e);
			}
		}
	}

}
