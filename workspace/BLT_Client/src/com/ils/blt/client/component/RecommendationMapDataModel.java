/**
 * Copyright 2014. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import prefuse.data.Graph;
import prefuse.data.Table;

import com.inductiveautomation.factorypmi.application.binding.VisionClientContext;
import com.inductiveautomation.ignition.common.Dataset;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.sfc.uimodel.ChartUIModel;

/** 
 * This class holds objects in a Tree structure. The tree is derived from
 * the input datasets. We recreate the tree whenever the datasets change.
 */
public class RecommendationMapDataModel {
	private static final String TAG = "RecommendationMapDataModel";
	private int ROOT_ROW = 0;           // Number of the root row
	// Table column names
	private static final String KIND    = "Kind";       // diagnosis, recommendation, output
	public static final String ID       = "Id";
	public static final String NAME     = "Name";
	public static final String ROW      = "Row";
	public static final String VALUE    = "Value";      // For recommendations, the factor
	
	// Node types
	public static final int DIAGNOSIS_KIND      = 0;
	public static final int RECOMMENDATION_KIND = 1;
	public static final int OUTPUT_KIND         = 2;
	
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final RecommendationMap recmap;
	private final Table nodes;
	private final Table edges;
	private final Map<Integer,Integer> diagnosisRowByKey;        
	private final Map<Integer,Integer> outputRowByKey;
	private final Map<String,Integer> recommendationRowByKey; // Key is concatenation of diagnosis:output
	
	public RecommendationMapDataModel(VisionClientContext ctx,RecommendationMap map) {
		recmap = map;

		nodes = new Table();
		nodes.addColumn(KIND, int.class);   // Count of linked connections
		nodes.addColumn(NAME, String.class);
		nodes.addColumn(ID,  int.class);          
		nodes.addColumn(ROW, int.class);            // Table row - key
		nodes.addColumn(VALUE, double.class);

		edges = new Table();
		// The keys match the node key in the node table
		// The node direction is from parent to child.
		edges.addColumn(Graph.DEFAULT_SOURCE_KEY, int.class);
		edges.addColumn(Graph.DEFAULT_TARGET_KEY, int.class);
		diagnosisRowByKey = new HashMap<>();
		outputRowByKey = new HashMap<>();
		recommendationRowByKey = new HashMap<>();
		initialize();
	}

	/**
	 * Initialize the UI
	 */
	private void initialize() {

		UUID root = ChartUIModel.ROOT_FOLDER;
		log.tracef("%s.initialize: ROOT_FOLDER = %s",TAG,root.toString());
		//configureRootNode();
		update();
	}
	/**
	 * Respond to changes in the dataset configuration by re-computing the graph.
	 */
	public void update() {
		// First make sure that all are defined
		if( recmap.getDiagnoses()==null || recmap.getRecommendations()==null || recmap.getOutputs()==null) return;
		
		Dataset diagnoses = recmap.getDiagnoses();
		int row = 0;
		while( row<diagnoses.getRowCount()) {
			try {
				Integer key = Integer.parseInt(diagnoses.getValueAt(row, RecommendationConstants.KEY_COLUMN).toString());
				int index = addNodeTableRow(DIAGNOSIS_KIND,diagnoses.getValueAt(row, RecommendationConstants.NAME_COLUMN).toString(),key);
				diagnosisRowByKey.put(key, new Integer(index));
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: diagnosis ID %s for %s is not a number (%s)",TAG,
						diagnoses.getValueAt(row, RecommendationConstants.KEY_COLUMN).toString(),
						diagnoses.getValueAt(row, RecommendationConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			row++;
		}
		Dataset outputs = recmap.getOutputs();
		row = 0;
		while( row<outputs.getRowCount()) {
			try {
				Integer key = Integer.parseInt(outputs.getValueAt(row, RecommendationConstants.KEY_COLUMN).toString());
				int index = addNodeTableRow(OUTPUT_KIND,outputs.getValueAt(row, RecommendationConstants.NAME_COLUMN).toString(),key);
				outputRowByKey.put(key, new Integer(index));
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: output ID %s for %s is not a number (%s)",TAG,
						outputs.getValueAt(row, RecommendationConstants.KEY_COLUMN).toString(),
						outputs.getValueAt(row, RecommendationConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			row++;
		}
		Dataset recommendations = recmap.getRecommendations();
		row = 0;
		while( row<recommendations.getRowCount()) {
			String key1 = recommendations.getValueAt(row, RecommendationConstants.DIAGNOSIS_KEY_COLUMN).toString();
			String key2 = recommendations.getValueAt(row, RecommendationConstants.OUTPUT_KEY_COLUMN).toString();
			String key = String.format("%s:%s",key1,key2);
			try {
				Double dbl = Double.parseDouble(recommendations.getValueAt(row, RecommendationConstants.VALUE_COLUMN).toString());
				int index = addRecNodeTableRow(recommendations.getValueAt(row, RecommendationConstants.NAME_COLUMN).toString(),key,dbl);
				recommendationRowByKey.put(key, new Integer(index));
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: recommended value %s for %s is not a number (%s)",TAG,
						recommendations.getValueAt(row, RecommendationConstants.VALUE_COLUMN).toString(),
						recommendations.getValueAt(row, RecommendationConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			
			row++;
		}
		

		// Create links
		row = 0;
		while( row<recommendations.getRowCount()) {
			String key1 = recommendations.getValueAt(row, RecommendationConstants.DIAGNOSIS_KEY_COLUMN).toString();
			String key2 = recommendations.getValueAt(row, RecommendationConstants.OUTPUT_KEY_COLUMN).toString();
			String key = String.format("%s:%s",key1,key2);
			Integer diagRow = diagnosisRowByKey.get(key1);
			Integer outRow  = outputRowByKey.get(key2);
			Integer recRow = recommendationRowByKey.get(key);
			if( recRow!=null) {
				if( diagRow!=null ) addEdgeTableRow(diagRow.intValue(),recRow.intValue());
				if( outRow!=null )  addEdgeTableRow(recRow.intValue(),outRow.intValue());
			}
			row++;
		}
	}
	
	/**
	 * @return a graph constructed out of the nodes and edges. It is a directed graph.
	 */
	public Graph getGraph() {
		Graph g = new Graph(nodes,edges,true,ROW,Graph.DEFAULT_SOURCE_KEY,Graph.DEFAULT_TARGET_KEY);
		return g;
	}
	

	
	// Create a connection between nodes
	// @return the row corresponding to the newly created connection.
	private int addEdgeTableRow(int sourceRow,int destinationRow) {
		int row = edges.getRowCount();
		log.infof("%s.addEdgeTableRow: %d -> %d", TAG,sourceRow,destinationRow);
		edges.addRow();
		edges.setInt(row,Graph.DEFAULT_SOURCE_KEY,sourceRow);
		edges.setInt(row,Graph.DEFAULT_TARGET_KEY,destinationRow);
		return row;
	}
	// Add a row to the nodes list
	// @return the number of the newly added row
	private int addNodeTableRow(int kind,String name,Integer key) {
		int row = nodes.getRowCount();
		log.infof("%s.addNodeTableRow: %d = %s", TAG,row,name);
		nodes.addRow();
		nodes.setInt(row,KIND,kind); 
		nodes.setString(row,NAME,name);
		nodes.setInt(row,ID,key);
		nodes.setInt(row,ROW,row);
		nodes.setDouble(row,VALUE,0.0);
		return row;
	}
	// Add a row to the nodes list
	// @return the number of the newly added row
	private int addRecNodeTableRow(String name,String key,Double value) {
		int row = nodes.getRowCount();
		log.infof("%s.addRecNodeTableRow: %d = %s", TAG,row,name);
		nodes.addRow();
		nodes.setInt(row,KIND,RECOMMENDATION_KIND); 
		nodes.setString(row,NAME,name);
		nodes.setInt(row,ROW,row);
		nodes.setDouble(row,VALUE,value);
		return row;
	}
	
	// Configure the nodes table to display something reasonable
	// if it is otherwise empty.
	private void configureRootNode() {
		ROOT_ROW = nodes.getRowCount();
		nodes.addRow();
		log.infof("%s.configureRootNode. root", TAG);
		nodes.setString(ROOT_ROW,NAME,"root");
		nodes.setInt(ROOT_ROW,KIND,0);
		nodes.setInt(ROOT_ROW,ID,ROOT_ROW);
		nodes.setInt(ROOT_ROW,ROW,ROOT_ROW);
		nodes.setDouble(ROOT_ROW,VALUE,0.0);
	}
	
}
