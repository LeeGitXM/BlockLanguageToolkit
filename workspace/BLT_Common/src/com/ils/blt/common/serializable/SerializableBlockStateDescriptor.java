package com.ils.blt.common.serializable;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * Use this class to transfer internal block state between a block
 * in the Gateway and a status screen in the Designer. We assume that
 * this is a one-way communication. It is the responsibility of the editor
 * make sense of the data transmitted.
 */
public class SerializableBlockStateDescriptor implements Serializable {
	private static final long serialVersionUID = 5499297358912286066L;
	private String name;
	private String idString = null;     // Block Id
	private Map<String,String> attributes;
	private List<Map<String,String>> buffer;
	
	public SerializableBlockStateDescriptor() {	
		name="UNSET";
		attributes = new HashMap<>();
		buffer = new ArrayList<>();
	}

	public String getName() {return name;}
	public void setName(String name) {this.name = name;}
	public String getIdString() {return idString;}
	public void setIdString(String idString) {this.idString = idString;}
	public Map<String, String> getAttributes() {return attributes;}
	public void setAttributes(Map<String, String> attributes) {this.attributes = attributes;}
	public List<Map<String, String>> getBuffer() {return buffer;}
	public void setBuffer(List<Map<String, String>> buffer) {this.buffer = buffer;}
}
