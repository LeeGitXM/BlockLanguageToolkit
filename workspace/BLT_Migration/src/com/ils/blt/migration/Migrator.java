/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import org.sqlite.JDBC;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.ObjectMapper;

public class Migrator {
	private static final String USAGE = "Usage: migrator <database>";
	private final Map<String,String> classMap;     // Lookup by G2 classname
	private final static JDBC driver = new JDBC(); // Force driver to be loaded
	private boolean ok = true;                     // Allows us to short circuit processing
	
	public Migrator() {
		classMap = new HashMap<String,String>();
	}
	
	public void processDatabase(String path) {
		String connectPath = "jdbc:sqlite:"+path;

		// Read database to generate conversion maps
		Connection connection = null;
		try {
			connection = DriverManager.getConnection(connectPath);
			Statement statement = connection.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.
			
			ResultSet rs = statement.executeQuery("select * from ClassMap");
			while(rs.next())
			{
				String g2 = rs.getString("G2Class");
				String ignition = rs.getString("IgnitionClass");
				classMap.put(g2, ignition);
			}
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(e.getMessage());
			ok = false;
		}
		finally {
			try {
				if(connection != null)
					connection.close();
			} 
			catch(SQLException e) {
				// connection close failed.
				System.err.println(e);
			}
		}
	}
	
	/**
	 * Read standard input. Convert into G2 Diagram
	 */
	public void processInput() {
		if( !ok ) return;
		
		// Read of stdin is expected to be from a re-directed file. 
		// We gobble the whole thing here.
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		StringBuffer input = new StringBuffer();
		String s = null;
		try{
			while ((s = in.readLine()) != null && s.length() != 0) {
				input.append(s);
			}
		}
		catch(IOException ignore) {}
		
		// Now convert into a G2 Diagram
		try {
			byte[] bytes = input.toString().getBytes();
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
			mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
			G2Diagram g2d = mapper.readValue(new String(bytes), G2Diagram.class);
			if( g2d==null ) {
				System.err.println("Failed to deserialize input");
				ok = false;
			}
		}
		catch( IOException ioe) {
			System.err.println(String.format("IOException (%s)",ioe.getLocalizedMessage())); 
			ok = false;
		}
		catch(Exception ex) {
			System.err.println(String.format("Deserialization exception (%s)",ex.getMessage()));
			ok = false;
		}

		
	}
	
	/**
	 * Convert from G2 objects into BLTView objects
	 */
	public void migrate() {
		
	}
	
	/**
	 * Write the BLT View Objects to std out
	 */
	public void createOutput() {
		
	}
	
	/**
	 * Entry point for the application. 
	 * Usage: Migrator <databasepath> 
	 * 
	 * NOTE: For Windows, specify path as: C:/home/work/migrate.db
	 *       For Mac/Linux:    /home/work/migrate.db
	 * We automatically adjust windows path, if specified with backslashes.
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args) {
			
		// Look for database path as an argument
		if( args.length == 0) {
			System.out.println(USAGE);
			System.exit(1);
		}
		
		Migrator m = new Migrator();
		String path = args[0];
		// In case we've been fed a Windows path, convert
		path = path.replace("\\", "/");
		m.processDatabase(path);
		m.processInput();
		m.migrate();
		m.createOutput();
		
		
	}

}
