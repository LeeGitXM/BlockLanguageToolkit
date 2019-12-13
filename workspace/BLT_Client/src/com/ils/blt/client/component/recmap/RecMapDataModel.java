/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import com.inductiveautomation.factorypmi.application.binding.VisionClientContext;
import com.inductiveautomation.ignition.common.Dataset;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.data.Graph;
import prefuse.data.Table;


/** 
 * This class holds objects in a Tree structure. The tree is derived from
 * the input datasets. We recreate the tree whenever the datasets change.
 */
public class RecMapDataModel {
	private static final String TAG = "RecMapDataModel";
	
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final RecommendationMap recmap;
	private final Table nodes;
	private final Table edges;
	private final Map<Integer,Integer> diagnosisNodeByDatasetRow;
	private final Map<Integer,Integer> outputNodeByDatasetRow;
	private final Map<String,Integer>recommendationRowByKey;
	private final Map<Integer,Properties> attributesByRow;
	private int sourceRowCount = 0;
	private int targetRowCount = 0;
	private int recommendationCount = 0;
	
	public RecMapDataModel(VisionClientContext ctx,RecommendationMap map) {
		recmap = map;

		nodes = new Table();
		nodes.addColumn(RecMapConstants.KIND, int.class);   // Count of linked connections
		nodes.addColumn(RecMapConstants.NAME, String.class);     
		nodes.addColumn(RecMapConstants.DSROW, int.class);    // Row from original dataset 
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
		diagnosisNodeByDatasetRow = new HashMap<>();
		outputNodeByDatasetRow = new HashMap<>();
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
	public Properties getAttributes(int row) { return attributesByRow.get(new Integer(row)); }
	/**
	 * @return the map of attributes by row
	 */
	public Map<Integer,Properties> getAttributeMap() { return attributesByRow; }
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
	 * 
	 */
	public void update() {
		// First make sure that all are defined
		if( recmap.getDiagnoses()==null || recmap.getRecommendations()==null || recmap.getOutputs()==null) return;
		
		Dataset diagnoses = recmap.getDiagnoses();
		sourceRowCount = diagnoses.getRowCount();
		int maxLength  = getMaxLength(diagnoses,RecMapConstants.NAME_COLUMN);
		int datasetRow = 0;
		while( datasetRow<diagnoses.getRowCount()) {
			try {
				int row = addNodeTableRow(RecMapConstants.SOURCE_KIND,datasetRow,
						                  padToMax(diagnoses.getValueAt(datasetRow, RecMapConstants.NAME_COLUMN).toString(),maxLength),datasetRow);
				diagnosisNodeByDatasetRow.put(new Integer(datasetRow), new Integer(row));
				Properties attributes = attributesByRow.get(new Integer(row));
				if( attributes==null ) {
					attributes = new Properties();
					attributesByRow.put(new Integer(row), attributes);
				}
				addAttribute(attributes,diagnoses,datasetRow,RecMapConstants.HAS_SQC);
				addAttribute(attributes,diagnoses,datasetRow,RecMapConstants.PROBLEM);
				addAttribute(attributes,diagnoses,datasetRow,RecMapConstants.MULTIPLIER);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: diagnosis ID %s for %s is not a number (%s)",TAG,
						  diagnoses.getValueAt(datasetRow, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warnf("%s.update: Exception reading diagnosis dataset (%s)",TAG,aiobe.getMessage());
			}
			datasetRow++;
		}
		
		Dataset outputs = recmap.getOutputs();
		targetRowCount = outputs.getRowCount();
		maxLength  = getMaxLength(outputs,RecMapConstants.NAME_COLUMN);
		datasetRow = 0;
		while( datasetRow<outputs.getRowCount()) {
			try {
				int row = addNodeTableRow(RecMapConstants.TARGET_KIND,datasetRow,
										  padToMax(outputs.getValueAt(datasetRow, RecMapConstants.NAME_COLUMN).toString(),maxLength),datasetRow);
				outputNodeByDatasetRow.put(new Integer(datasetRow), new Integer(row));
				Properties attributes = attributesByRow.get(new Integer(row));
				if( attributes==null ) {
					attributes = new Properties();
					attributesByRow.put(new Integer(row), attributes);
				}
				addAttribute(attributes,outputs,datasetRow,RecMapConstants.CURRENT);
				addAttribute(attributes,outputs,datasetRow,RecMapConstants.FINAL);
				addAttribute(attributes,outputs,datasetRow,RecMapConstants.RECOMMENDATION);
				addAttribute(attributes,outputs,datasetRow,RecMapConstants.TARGET);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: output ID %s for %s is not a number (%s)",TAG,
						outputs.getValueAt(datasetRow, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warnf("%s.update: Exception reading outputs dataset (%s)",TAG,aiobe.getMessage());
			}
			datasetRow++;
		}
		
		Dataset recommendations = recmap.getRecommendations();
		recommendationCount = recommendations.getRowCount();
		maxLength  = getMaxLength(recommendations,RecMapConstants.NAME_COLUMN);
		
		datasetRow = 0;
		while( datasetRow<recommendations.getRowCount()) {
			int sourceRow = Integer.parseInt(recommendations.getValueAt(datasetRow, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString());
			int targetRow = Integer.parseInt(recommendations.getValueAt(datasetRow, RecMapConstants.OUTPUT_ID_COLUMN).toString());
			int row = addRecommendationNodeTableRow(datasetRow,sourceRow,targetRow);
			String key = String.format("%d:%d",sourceRow,targetRow);
			recommendationRowByKey.put(key, new Integer(row));
			log.warnf("%s.update: Added recommendation node %d for key %s",TAG,row,key);
			Properties attributes = attributesByRow.get(new Integer(row));
			if( attributes==null ) {
				attributes = new Properties();
				attributesByRow.put(new Integer(row), attributes);
			}
			addAttribute(attributes,recommendations,datasetRow,RecMapConstants.MANUAL);
			addAttribute(attributes,recommendations,datasetRow,RecMapConstants.AUTO);
			addAttribute(attributes,recommendations,datasetRow,RecMapConstants.IS_AUTO);
			datasetRow++;
		}
		
		// Create links -- connections
		Dataset connections = recmap.getConnections();
		datasetRow = 0;
		while( datasetRow<connections.getRowCount()) {
			try {
				int key1 = Integer.parseInt(connections.getValueAt(datasetRow, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString());
				int key2 = Integer.parseInt(connections.getValueAt(datasetRow, RecMapConstants.OUTPUT_ID_COLUMN).toString());
				Boolean active =  (Boolean)connections.getValueAt(datasetRow, RecMapConstants.ACTIVE);
				
				Integer diagRow = diagnosisNodeByDatasetRow.get(new Integer(key1));  
				Integer outRow  = outputNodeByDatasetRow.get(new Integer(key2));
				// If not active, connect source and target directly
				// otherwise connect through the recommendation block
				if( !active.booleanValue() && diagRow!=null && outRow!=null) {
					addEdgeTableRow(diagRow.intValue(),outRow.intValue(),false);
					log.infof("%s.update: Created straight thru edge from %d to %d",TAG,diagRow.intValue(),outRow.intValue());
				}
				else if( active.booleanValue() && diagRow!=null && outRow!=null) {
					String key3 = String.format("%d:%d",key1,key2);
					Integer recRow  = recommendationRowByKey.get(key3);
					if( recRow!=null ) {
						log.infof("%s.update: Created rec node edge from %d to %d  (%s)",TAG,diagRow.intValue(),recRow.intValue(),key3);
						addEdgeTableRow(diagRow.intValue(),recRow.intValue(),true);
						addEdgeTableRow(recRow.intValue(),outRow.intValue(),true);
					}
					else{
						log.warnf("%s.update: Recommendation node not found for key %s",TAG,key3);
					}
				}
				else if( diagRow==null) {
					log.warnf("%s.update: Diagnostic node not found for key %d",TAG,key1);
				}
				else if( outRow==null) {
					log.warnf("%s.update: Output node not found for key %d",TAG,key2);
				}
				else {
					log.warnf("%s.update: ERROR: Should not have reached this point...",TAG);
				}
				
			}
			catch(ArrayIndexOutOfBoundsException aiobe) {
				log.warn(String.format("%s.update: Index exception reading connection dataset (%s)",TAG,aiobe.getLocalizedMessage(),aiobe));
			}
			catch(Exception ex) {
				log.warn(String.format("%s.update: Exception reading connection dataset (%s)",TAG,ex.getLocalizedMessage(),ex));
			}
			datasetRow++;
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

	
	private void addAttribute(Properties attributes,Dataset ds,int dsrow,String colName) {
		String value = "";
		try {
			Object obj = ds.getValueAt(dsrow, colName);
			if( obj!=null) value = obj.toString();
		}
		catch(ArrayIndexOutOfBoundsException ignore) {}  // Column not in dataset
		attributes.setProperty(colName,value) ;
	}
	
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
		nodes.setInt(row,RecMapConstants.DSROW,datasetRow);
		nodes.setInt(row,RecMapConstants.ROW,row);
		return row;
	}
	// Add a row to the nodes list. SOURCEROW/TARGETROW references are to dataset ids.
	// @return the number of the newly added row
	private int addRecommendationNodeTableRow(int datasetRow,int source,int target) {
		int row = nodes.getRowCount();
		log.debugf("%s.addRecNodeTableRow: %d = (%d->%d)", TAG,row,source,target);
		nodes.addRow();
		nodes.setInt(row,RecMapConstants.KIND,RecMapConstants.INFO_KIND); 
		nodes.setString(row,RecMapConstants.NAME,"Rec");
		nodes.setInt(row,RecMapConstants.DSROW,datasetRow);
		nodes.setInt(row,RecMapConstants.ROW,row);
		nodes.setInt(row,RecMapConstants.SOURCEROW,source);
		nodes.setInt(row,RecMapConstants.TARGETROW,target);
		return row;
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
