package com.ils.blt.designer.applicationConfiguration;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

public class Application {
	private String name;
	private Integer newOutputId = -1;
	private String description = "";
	private String console ="";
	private ArrayList<String> consoles;
	private String queue = "";
	private ArrayList<String> queues;
	private ArrayList< Map<String,Object> > outputList;
	private String groupRampMethod = "";
	private String post = "";
	private String unit = "";
	
	// Constructor
	public Application() {
		consoles = new ArrayList<>();
		outputList = new ArrayList< Map<String,Object> >();
		queues = new ArrayList<>();
	}
	
	// Getters
	public String getName() { return name; }
	public String getDescription() {return description;}
	public String getConsole() {return console;}
	public List<String> getConsoles() {return consoles;}
	public String getQueue() {return queue;}
	public List<String> getQueues() {return queues;}
	public String getGroupRampMethod() {return groupRampMethod;}
	public String getPost() {return post;}
	public String getUnit() {return unit;}
	public List< Map<String,Object> > getOutputs() {return outputList;}

	@SuppressWarnings("unchecked")
	public Map<String,Object> getOutput(String outputName) {
		System.out.println("Getting " + outputName + " from output list: " + outputList);
		Map<String,Object> outputMap = null;
		for(Object obj : outputList) {
			outputMap = (Map<String,Object>) obj;
			String str = (String) outputMap.get("QuantOutput");
			if (str.equals(outputName)){
				return outputMap;
			}
		}
		return null;
	}

	// Setters
	public void setName(String nam) { name=nam;}
	public void setDescription(String desc) {description = desc;}
	public void setConsole(String cons) {console = cons;}
	public void setConsoles(ArrayList<String> cons) {consoles=cons;}
	public void setQueue(String q) {queue = q;}
	public void setQueues(ArrayList<String> qs) {queues=qs;}
	public void setGroupRampMethod(String m) {groupRampMethod = m;}
	public void setPost(String p) {post = p;}
	public void setUnit(String u) {unit = u;}
	public void setOutputs(ArrayList< Map<String,Object> > o) {outputList=o;}

	// Adding an output 
	public void addQuantOutput(Map<String,Object> map){
		String quantOutput = (String) map.get("QuantOutput");
		outputList.add(map);
	}
		
	public void deleteQuantOutput(String outputName){
		System.out.println("Removing output " + outputName);
		Map<String,Object> outputMap=getOutput(outputName);
		outputList.remove(outputMap);
	}

	// Create a new outputMap, which corresponds to a QuantOutput, with default values
	public Map<String,Object> newOutput() {
		System.out.println("Creating a new output... ");
		
		Hashtable<String,Object> outputMap = new Hashtable<String,Object>();
		outputMap.put("QuantOutputId", newOutputId);
		newOutputId = newOutputId - 1;
		outputMap.put("QuantOutput", "");
		outputMap.put("TagPath", "");
		outputMap.put("MostNegativeIncrement", -10.0);
		outputMap.put("MostPositiveIncrement", 10.0);
		outputMap.put("MinimumIncrement", 0.01);
		outputMap.put("SetpointLowLimit", 0.0);
		outputMap.put("SetpointHighLimit", 100.0);
		outputMap.put("FeedbackMethod", "SIMPLE-SUM");
		outputMap.put("IncrementalOutput", true);
		
		outputList.add(outputMap);
		return outputMap;
	}
}