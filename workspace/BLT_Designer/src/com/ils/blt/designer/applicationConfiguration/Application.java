package com.ils.blt.designer.applicationConfiguration;

import java.util.ArrayList;
import java.util.List;

public class Application {
	private String name;
	private String description;
	private String console;
	private ArrayList<String> consoles;
	private String queue;
	private ArrayList<String> queues;
	private ArrayList<String> outputList;

	// Getters
	public String getName() { return name; }
	public String getDescription() {return description;}
	public String getConsole() {return console;}
	public ArrayList<String> getConsoles() {return consoles;}
	public String getQueue() {return queue;}
	public ArrayList<String> getQueues() {return queues;}
	
	// Setters
//	public void setName(String nam) { if(nam!=null) name=nam;}
	public void setName(String nam) { name=nam;}
	public void setDescription(String desc) {description = desc;}
	public void setConsole(String cons) {console = cons;}
	public void setConsoles(ArrayList<String> cons) {consoles=cons;}
	public void setQueue(String q) {queue = q;}
	public void setQueues(ArrayList<String> qs) {queues=qs;}
	
	public void addQuantOutput(String Name, String Tag, Float mostNegativeIncrement){
		System.out.println("Adding output " + Name);
	}
	
	public void deleteQuantOutput(String Name){
		System.out.println("Removing output " + Name);
	}
	
}
