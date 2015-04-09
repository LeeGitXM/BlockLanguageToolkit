/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.sqlite.JDBC;

import com.ils.block.AbstractProcessBlock;

/**
 * Read the tag mapping database table and create a tag import file
 * for Ignition.
 * @author chuckc
 *
 */
public class TagMigrator {
	private final static String TAG = "TagMigrator";
	private static final String USAGE = "Usage: tag_migrator <database>";
	private final List<String> paths;  // Parent folder paths
	private final List<TagData> tags;
	@SuppressWarnings("unused")
	private final static JDBC driver = new JDBC(); // Force driver to be loaded
	 
	public TagMigrator() {
		paths = new ArrayList<>();
		tags = new ArrayList<TagData>();
	}
	
	public void processDatabase(String path) {
		String connectPath = "jdbc:sqlite:"+path;

		// Read database to generate conversion maps
		@SuppressWarnings("resource")
		Connection connection = null;
		try {
			connection = DriverManager.getConnection(connectPath);
			createList(connection);
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+": "+e.getMessage());
		}
		finally {
			try {
				if(connection != null)
					connection.close();
			} 
			catch(SQLException e) {
				// connection close failed.
				System.err.println(TAG+": "+e.getMessage());
			}
		}
	}
	
	
	/**
	 * Perform a database lookup to create a map of G2
	 * block names to Ignition blocks.
	 * @param cxn open database connection
	 */
	private void createList(Connection cxn) {
		@SuppressWarnings("resource")
		ResultSet rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.
			
			rs = statement.executeQuery("select * from TagMap");
			while(rs.next())
			{
				String tagPath = rs.getString("TagPath");
				String type = rs.getString("DataType");
				TagData td = new TagData(tagPath,type);
				if( !tags.contains(td) ) tags.add(td);
			}
			rs.close();
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
	}
	

	
	/**
	 * Iterate through the list of tags and write to std out as
	 * an XML file, suitable for import into Ignition.
	 *   
	 */
	public void createOutput() {
		System.out.println("<Tags>");
		for( TagData td:tags) {
			// Strip provider
			String tagPath = td.getPath();
			int pos = tagPath.indexOf("]");
			if( pos>=0 ) tagPath = tagPath.substring(pos+1);
			String originalPath = tagPath;
			pos = tagPath.indexOf("/");
			String path = "";
			String subPath = "";
			while( pos>0 ) {
				String dir = tagPath.substring(0, pos);  // Next segment
				if(! path.isEmpty() ) path = path+"/";
				path = path+dir;  
				if(!paths.contains(path)) {
					paths.add(path);
					System.out.println("<Tag name=\""+dir+"\" path=\""+subPath+"\" type=\"Folder\"/>");
				}
				subPath = path;
				tagPath = tagPath.substring(pos+1);
				pos = tagPath.indexOf("/");
			}
			// When writing the tag, use the entire parent directory
			String tagName = tagPath;
			tagPath = td.getPath();
			pos = originalPath.lastIndexOf("/");
			if( pos>0 ) path = originalPath.substring(0, pos);
			else path = "";
			System.out.println("<Tag name=\""+tagName+"\" path=\""+path+"\" type=\"DB\">");
			System.out.println("<Property name=\"DataType\">"+td.getTypeCode()+"</Property>");
			System.out.println("</Tag>");
		}
		System.out.println("</Tags>");
	}
	/**
	 * Store metadata re: a tag path
	 * Types:
	 *   2 - Int
	 *   5 - Float
	 *   6 - Boolean
	 *   7 - String
	 */
	private class TagData {
		private final String path;
		private final String type;
		public TagData(String path,String type) {
			this.path = path;
			this.type = type;
			
		}
		public String getPath() {return path;}
		public String getTypeCode() {
			String tc = type;
			if( type.equalsIgnoreCase("Double")) tc = "5";
			else if( type.equalsIgnoreCase("Integer")) tc = "2";
			else if( type.equalsIgnoreCase("Boolean")) tc = "6";
			else if( type.equalsIgnoreCase("String")) tc = "7";
			return tc;
		}
		
		// So that class is comparable
		// Same tagPath is sufficient to prove equality
		@Override
		public boolean equals(Object arg) {
			boolean result = false;
			if( arg instanceof TagData) {
				TagData that = (TagData)arg;
				if( this.getPath().equals(that.getPath()) ) {
					result = true;
				}
			}
			return result;
		}
		@Override
		public int hashCode() {
			return path.hashCode();
		}
	}
	
	/**
	 * Entry point for the application. 
	 * Usage: TagMigrator <databasepath> 
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
		// Some of the embedded jars use log4j - redirect to std error. Log level is system property "log.level"
		ConsoleAppender appender = new ConsoleAppender(new PatternLayout(PatternLayout.TTCC_CONVERSION_PATTERN),"System.err");
		BasicConfigurator.configure(appender);
		String levelString = System.getProperty("log.level");
		Level level = Level.WARN;
		if( levelString!=null) level = Level.toLevel(levelString);
        Logger.getRootLogger().setLevel(level); //set log level
        
      
		TagMigrator m = new TagMigrator();
		String path = args[0];
		// In case we've been fed a Windows path, convert
		path = path.replace("\\", "/");
		m.processDatabase(path);
		m.createOutput();
	}

}
