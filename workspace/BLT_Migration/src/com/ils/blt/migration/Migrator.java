/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.sqlite.JDBC;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;

public class Migrator {
	private static final String USAGE = "Usage: migrator <database>";
	@SuppressWarnings("unused")
	private final static JDBC driver = new JDBC(); // Force driver to be loaded
	private boolean ok = true;                     // Allows us to short circuit processing
	private G2Diagram g2diagram = null;                  // G2 Diagram read from JSON
	private SerializableDiagram diagram = null;    // The result
	private final ClassMapper classMapper;
	 
	public Migrator() {
		classMapper = new ClassMapper();
	}
	
	public void processDatabase(String path) {
		String connectPath = "jdbc:sqlite:"+path;

		// Read database to generate conversion maps
		Connection connection = null;
		try {
			connection = DriverManager.getConnection(connectPath);
			classMapper.createMap(connection);
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
			g2diagram = mapper.readValue(new String(bytes), G2Diagram.class);
			if( g2diagram==null ) {
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
		if( !ok ) return;
		
		diagram = new SerializableDiagram();
		diagram.setName(g2diagram.getName());
		List<SerializableBlock> blocks = new ArrayList<SerializableBlock>();
		for( G2Block g2block:g2diagram.getBlocks()) {
			SerializableBlock block = new SerializableBlock();
			block.setId(g2block.getId());
			block.setOriginalId(g2block.getId());
			block.setLabel(g2block.getLabel());
			block.setX(g2block.getX());
			block.setY(g2block.getY());
			classMapper.setClassName(g2block, block);
			
			blocks.add(block);
			diagram.setBlocks(blocks.toArray(new SerializableBlock[blocks.size()]));
		}
	}
	
	/**
	 * Write the BLT View Objects to std out
	 */
	public void createOutput() {
		if( !ok ) return;
		
		ObjectMapper mapper = new ObjectMapper();
		try{ 
			String json = mapper.writeValueAsString(diagram);
			System.out.println(json);
		}
		catch(JsonProcessingException jpe) {
			System.err.println("Unable to serialize migrated diagram");
		}
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
