package com.ils.blt.migration;

/**
 * Hold an attribute of a block as a name-value pair.
 */
public class G2Property {
	private String name;
	private Object value;

	/** 
	 * Constructor: Sets all attributes.
	 */
	public G2Property(String name,Object value) {
		this.name = name;
		this.value = value;
	}
	
	public G2Property() {
	}
	
	
	public String getName() {return name;}
	public void setName(String name) {this.name = name;}
	public Object getValue() {return value;}
	public void setValue(Object value) {this.value = value;}
	
}
