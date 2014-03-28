package com.ils.blt.migration;



/**
 * Implement a plain-old-java-object representing a G2 Application
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class G2Application {
	private G2Family[] families;
	private String name;
	
	public G2Application() {	
		families = new G2Family[0];
		name="UNSET";
	}
	
	public G2Family[] getFamilies() { return families; }
	public String getName() { return name; }

	public void setFamilies(G2Family[] list) { families=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
}
