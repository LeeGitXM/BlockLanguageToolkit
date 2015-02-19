package com.ils.blt.designer.applicationConfiguration;

import java.util.List;

public class Application {
	private String name;
	private String description;
	private String console;
	private String queue;
	private List outputList;

	// Getters
	public String getName() { return name; }
	public String getDescription() {return description;}
	public String getConsole() {return console;}
	public String getQueue() {return queue;}
	
	// Setters
//	public void setName(String nam) { if(nam!=null) name=nam;}
	public void setName(String nam) { name=nam;}
	public void setDescription(String desc) {description = desc;}
	public void setConsole(String cons) {console = cons;}
	public void setQueue(String q) {queue = q;}
	
	public void addQuantOutput(String Name, String Tag, Float mostNegativeIncrement){
		System.out.println("Adding output " + Name);
	}
	
	public void deleteQuantOutput(String Name){
		System.out.println("Removing output " + Name);
	}
	
}
