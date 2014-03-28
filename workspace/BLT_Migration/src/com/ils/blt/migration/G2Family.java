package com.ils.blt.migration;



/**
 * Implement a plain-old-java-object representing a G2 family of diagrams
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class G2Family {
	private G2Diagram[] diagrams;
	private String name;
	
	public G2Family() {	
		diagrams = new G2Diagram[0];
		name="UNSET";
	}
	
	public G2Diagram[] getDiagrams() { return diagrams; }
	public String getName() { return name; }

	public void setDiagrams(G2Diagram[] list) { diagrams=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
}
