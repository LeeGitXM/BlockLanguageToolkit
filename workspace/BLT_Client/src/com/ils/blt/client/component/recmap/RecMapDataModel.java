/**
 * Copyright 2014. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.inductiveautomation.factorypmi.application.binding.VisionClientContext;
import com.inductiveautomation.ignition.common.Dataset;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.Visualization;
import prefuse.data.Graph;
import prefuse.data.Table;


/** 
 * This class holds objects in a Tree structure. The tree is derived from
 * the input datasets. We recreate the tree whenever the datasets change.
 */
public class RecMapDataModel {
	private static final String TAG = "RecommendationMapDataModel";
	
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final RecommendationMap recmap;
	private final Table nodes;
	private final Table edges;
	// "Key" is the "Id" value in the dataset.
	// Map by "TableRow" holds the id of the node  
	private final Map<Integer,Integer> diagnosisNodesRowByKey;        
	private final Map<Integer,Integer> outputNodesRowByKey;
	private final Map<String,Integer> recommendationRowByKey; // Key is concatenation of diagnosis:output
	private final Map<Integer,List<NameValuePair>> attributesByRow;
	private int sourceRowCount = 0;
	private int targetRowCount = 0;
	private int recommendationCount = 0;
	
	public RecMapDataModel(VisionClientContext ctx,RecommendationMap map) {
		recmap = map;

		nodes = new Table();
		nodes.addColumn(RecMapConstants.KIND, int.class);     // Count of linked connections
		nodes.addColumn(RecMapConstants.NAME, String.class);
		nodes.addColumn(RecMapConstants.ID,  int.class);       
		nodes.addColumn(RecMapConstants.INDEX, int.class);    // Row from original dataset 
		nodes.addColumn(RecMapConstants.ROW, int.class);          
		nodes.addColumn(RecMapConstants.SOURCEROW, int.class); 
		nodes.addColumn(RecMapConstants.TARGETROW, int.class); 

		edges = new Table();
		// The keys match the node key in the node table
		// The node direction is from parent to child.
		edges.addColumn(Graph.DEFAULT_SOURCE_KEY, int.class);
		edges.addColumn(Graph.DEFAULT_TARGET_KEY, int.class);
		edges.addColumn(RecMapConstants.ACTIVE, boolean.class);
		attributesByRow = new HashMap<>();
		diagnosisNodesRowByKey = new HashMap<>();
		outputNodesRowByKey = new HashMap<>();
		recommendationRowByKey = new HashMap<>();
		initialize();
	}

	/**
	 * Initialize the UI
	 */
	private void initialize() {
		update();
	}
	/**
	 * @return a list of attributes for a given node
	 */
	public List<NameValuePair> getAttributes(int row) { return attributesByRow.get(new Integer(row)); }
	/**
	 * @return all of the edges
	 */
	public Table getEdges() { return this.edges; }
	/**
	 * @return all of the nodes
	 */
	public Table getNodes() { return this.nodes; }
	
	/**
	 * Respond to changes in the dataset configuration by re-computing the graph.
	 */
	public void update() {
		// First make sure that all are defined
		if( !recmap.isConfigured()) return;
		
		// Diagnoses
		Dataset diagnoses = recmap.getDiagnoses();
		sourceRowCount = diagnoses.getRowCount();
		int maxLength  = getMaxLength(diagnoses,RecMapConstants.NAME_COLUMN);
		int row = 0;
		while( row<diagnoses.getRowCount()) {
			
			try {
				int key = Integer.parseInt(diagnoses.getValueAt(row, RecMapConstants.ID_COLUMN).toString());
				int index = addNodeTableRow(RecMapConstants.SOURCE_KIND,row,
						padToMax(diagnoses.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(),maxLength),key);
				diagnosisNodesRowByKey.put(new Integer(key), new Integer(index));
				List<NameValuePair> attributes = attributesByRow.get(new Integer(index));
				if( attributes==null ) {
					attributes = new ArrayList<>();
					attributesByRow.put(new Integer(index), attributes);
				}
				addAttribute(attributes,diagnoses,row,RecMapConstants.PROBLEM);
				addAttribute(attributes,diagnoses,row,RecMapConstants.MULTIPLIER);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: diagnosis ID %s for %s is not a number (%s)",TAG,
						diagnoses.getValueAt(row, RecMapConstants.ID_COLUMN).toString(),
						diagnoses.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warnf("%s.update: Exception reading diagnosis dataset (%s)",TAG,aiobe.getMessage());
			}
			row++;
		}
		
		// Quant Outputs
		Dataset outputs = recmap.getOutputs();
		targetRowCount = outputs.getRowCount();
		maxLength  = getMaxLength(outputs,RecMapConstants.NAME_COLUMN);
		row = 0;
		while( row<outputs.getRowCount()) {
			try {
				int key = Integer.parseInt(outputs.getValueAt(row, RecMapConstants.ID_COLUMN).toString());
				int index = addNodeTableRow(RecMapConstants.TARGET_KIND,row,
						padToMax(outputs.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(),maxLength),key);
				outputNodesRowByKey.put(new Integer(key), new Integer(index));
				List<NameValuePair> attributes = attributesByRow.get(new Integer(index));
				if( attributes==null ) {
					attributes = new ArrayList<>();
					attributesByRow.put(new Integer(row), attributes);
				}
				addAttribute(attributes,outputs,row,RecMapConstants.CURRENT);
				addAttribute(attributes,outputs,row,RecMapConstants.FINAL);
				addAttribute(attributes,outputs,row,RecMapConstants.RECOMMENDATION);
				addAttribute(attributes,outputs,row,RecMapConstants.TARGET);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: output ID %s for %s is not a number (%s)",TAG,
						outputs.getValueAt(row, RecMapConstants.ID_COLUMN).toString(),
						outputs.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warnf("%s.update: Exception reading outputs dataset (%s)",TAG,aiobe.getMessage());
			}
			row++;
		}
		
		// Recommendations
		Dataset recommendations = recmap.getRecommendations();
		recommendationCount = recommendations.getRowCount();
		maxLength  = getMaxLength(recommendations,RecMapConstants.NAME_COLUMN);
		
		row = 0;
		while( row<recommendations.getRowCount()) {
			try {
				int key1 = Integer.parseInt(recommendations.getValueAt(row, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString());
				Integer source = diagnosisNodesRowByKey.get(new Integer(key1));
				int key2 = Integer.parseInt(recommendations.getValueAt(row, RecMapConstants.OUTPUT_ID_COLUMN).toString());
				Integer target = outputNodesRowByKey.get(new Integer(key2));
				if( source!=null && target!=null ) {
					String key = String.format("%d:%d",key1,key2);

					int index = addRecommendationNodeTableRow(row,source.intValue(),target.intValue());
					recommendationRowByKey.put(key, new Integer(index));
					//log.infof("%s.update: added %d for %s to rec map",TAG,index,key);
					List<NameValuePair> attributes = attributesByRow.get(new Integer(index));
					if( attributes==null ) {
						attributes = new ArrayList<>();
						attributesByRow.put(new Integer(row), attributes);
					}
					addAttribute(attributes,outputs,row,RecMapConstants.AUTO);
				}
				else {
					log.warnf("%s.update: Recommendations %s has incorrect source %d, or target %d reference",TAG,
							recommendations.getValueAt(row, RecMapConstants.NAME).toString(),key1,key2);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: diagnosisId %s or outputId %s in recommendation is not a number (%s)",TAG,
						recommendations.getValueAt(row, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString(),
						recommendations.getValueAt(row, RecMapConstants.OUTPUT_ID_COLUMN).toString(), nfe.getMessage());
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warnf("%s.update: Exception reading recommendation dataset (%s)",TAG,aiobe.getMessage());
			}

			row++;
		}
		

		// Create links -- connections
		Dataset connections = recmap.getConnections();
		row = 0;
		while( row<connections.getRowCount()) {
			try {
				int key1 = Integer.parseInt(connections.getValueAt(row, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString());
				int key2 = Integer.parseInt(connections.getValueAt(row, RecMapConstants.OUTPUT_ID_COLUMN).toString());
				boolean active = Boolean.parseBoolean(connections.getValueAt(row, RecMapConstants.ACTIVE).toString());
				Integer diagRow = diagnosisNodesRowByKey.get(new Integer(key1));
				Integer outRow  = outputNodesRowByKey.get(new Integer(key2));
				if( diagRow!=null&&outRow!=null) {
					addEdgeTableRow(diagRow.intValue(),outRow.intValue(),active);
				}
				else if( diagRow==null) {
					log.warnf("%s.update: Diagnostic node not found for key %s",TAG,key1);
				}
				else{
					log.warnf("%s.update: Output node not found for key %s",TAG,key2);
				}
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warnf("%s.update: Exception reading connection dataset (%s)",TAG,aiobe.getMessage());
			}
			row++;
		}
	}
	
	/**
	 * @return a graph constructed out of the nodes and edges. It is a directed graph.
	 */
	public Graph getGraph() {
		Graph g = new Graph(nodes,edges,true,RecMapConstants.ROW,Graph.DEFAULT_SOURCE_KEY,Graph.DEFAULT_TARGET_KEY);
		return g;
	}
	
	public int getSourceRowCount() { return sourceRowCount; }
	public int getTargetRowCount() { return targetRowCount; }
	public int getRecommendationCount() { return recommendationCount; }

	
	// Create a connection between nodes
	// @return the row corresponding to the newly created connection.
	private int addEdgeTableRow(int sourceRow,int destinationRow,boolean active) {
		int row = edges.getRowCount();
		log.debugf("%s.addEdgeTableRow: %d -> %d", TAG,sourceRow,destinationRow);
		edges.addRow();
		edges.setInt(row,Graph.DEFAULT_SOURCE_KEY,sourceRow);
		edges.setInt(row,Graph.DEFAULT_TARGET_KEY,destinationRow);
		edges.setBoolean(row, RecMapConstants.ACTIVE, active);
		
		return row;
	}
	// Add a row to the nodes list
	// @return the number of the newly added row
	private int addNodeTableRow(int kind,int datasetRow,String name,Integer key) {
		int row = nodes.getRowCount();
		log.debugf("%s.addNodeTableRow: %d = %s", TAG,row,name);
		nodes.addRow();
		nodes.setInt(row,RecMapConstants.KIND,kind); 
		nodes.setString(row,RecMapConstants.NAME,name);
		nodes.setInt(row,RecMapConstants.ID,key);
		nodes.setInt(row,RecMapConstants.INDEX,datasetRow);
		nodes.setInt(row,RecMapConstants.ROW,row);
		return row;
	}
	// Add a row to the nodes list
	// @return the number of the newly added row
	private int addRecommendationNodeTableRow(int datasetRow,int source,int target) {
		int row = nodes.getRowCount();
		log.debugf("%s.addRecNodeTableRow: %d = (%d->%d)", TAG,row,source,target);
		nodes.addRow();
		nodes.setInt(row,RecMapConstants.KIND,RecMapConstants.INFO_KIND); 
		nodes.setInt(row,RecMapConstants.INDEX,datasetRow);
		nodes.setInt(row,RecMapConstants.ROW,row);
		nodes.setInt(row,RecMapConstants.SOURCEROW,source);
		nodes.setInt(row,RecMapConstants.TARGETROW,target);
		return row;
	}
	
	private void addAttribute(List<NameValuePair> attributes,Dataset ds,int row,String colName) {
		String value = "";
		try {
			Object obj = ds.getValueAt(row, colName);
			if( obj!=null) value = obj.toString();
		}
		catch(ArrayIndexOutOfBoundsException ignore) {}  // Column not in dataset
		attributes.add(new NameValuePair(colName,value)) ;
	}
	
	private int getMaxLength(Dataset ds,String colName) {
		int result = 0;
		int row = 0;
		int rowCount = ds.getRowCount();
		while( row<rowCount) {
			int len = 0;
			try {
				len = ds.getValueAt(row, colName).toString().length();
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.debugf("%s.getMaxLength: Dataset missing column %s", TAG,colName);
			}
			if( len>result) result = len; 
			row++;
		}
		return result;
	}
	
	private String padToMax(String in,int max) {
		String result = in + "                                      ";   // Max width 40
		return result.substring(0, max);
	}
}
