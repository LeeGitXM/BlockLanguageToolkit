package com.ils.blt.designer.applicationConfiguration;

public class Application {
	private String name;
	private String description;
	private String console;

	// Getters
	public String getName() { return name; }
	public String getDescription() {return description;}
	public String getConsole() {return console;}
	
	// Setters
//	public void setName(String nam) { if(nam!=null) name=nam;}
	public void setName(String nam) { name=nam;}
	public void setDescription(String desc) {description = desc;}
	public void setConsole(String cons) {console = cons;}
}
