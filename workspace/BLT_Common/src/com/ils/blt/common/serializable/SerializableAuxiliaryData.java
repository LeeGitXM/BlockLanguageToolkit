/**
 *   (c) 2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.serializable;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * Use this class to transfer auxiliary data to and from a block.
 * The permissible data structures are contained in 3 maps:
 *    properties - Map<String,String>
 *    lists      - Map<String,List<String>>
 *    maps       - Map<String,Map<String,String>>
 * This structure is designed to be easily serializable.
 */
public class SerializableAuxiliaryData implements Serializable {
	private static final long serialVersionUID = 5499297358912286066L;
	private Map<String,String> properties;
	private Map<String,List<String>> lists;
	private Map<String,List<Map<String,String>>> maplists;
	
	public SerializableAuxiliaryData() {	;
		properties = new HashMap<>();
		lists = new HashMap<>();
		maplists  = new HashMap<>();
	}

	public Map<String,String> getProperties() {return properties;}
	public void setProperties(Map<String,String> map) {this.properties = map;}
	public Map<String,List<String>> getLists() {return lists;}
	public void setLists(Map<String,List<String>> map) {this.lists = map;}
	public Map<String,List<Map<String,String>>> getMapLists() {return maplists;}
	public void setMapLists(Map<String,List<Map<String,String>>> list) {this.maplists = list;}
}
