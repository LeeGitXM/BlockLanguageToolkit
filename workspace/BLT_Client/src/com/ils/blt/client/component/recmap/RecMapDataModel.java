/**
 * Copyright 2014. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;
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
public class RecMapDataModel {
	private static final String TAG = "RecommendationMapDataModel";
	
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final RecommendationMap recmap;
	private final Table nodes;
	private final Table edges;
	private final Map<Integer,Integer> diagnosisGridRowByKey;   
	private final Map<Integer,Integer> diagnosisTableRowByKey;        
	private final Map<Integer,Integer> outputGridRowByKey;
	private final Map<Integer,Integer> outputTableRowByKey;
	private final Map<String,Integer> recommendationRowByKey; // Key is concatenation of diagnosis:output
	private int sourceRowCount = 0;
	private int targetRowCount = 0;
	private int recommendationCount = 0;
	
	public RecMapDataModel(VisionClientContext ctx,RecommendationMap map) {
		recmap = map;

		nodes = new Table();
		nodes.addColumn(RecMapConstants.KIND, int.class);   // Count of linked connections
		nodes.addColumn(RecMapConstants.NAME, String.class);
		nodes.addColumn(RecMapConstants.ID,  int.class);          
		nodes.addColumn(RecMapConstants.ROW, int.class);         
		nodes.addColumn(RecMapConstants.SOURCEROW, int.class); 
		nodes.addColumn(RecMapConstants.TARGETROW, int.class); 
		nodes.addColumn(RecMapConstants.VALUE, double.class);

		edges = new Table();
		// The keys match the node key in the node table
		// The node direction is from parent to child.
		edges.addColumn(Graph.DEFAULT_SOURCE_KEY, int.class);
		edges.addColumn(Graph.DEFAULT_TARGET_KEY, int.class);
		diagnosisGridRowByKey = new HashMap<>();
		diagnosisTableRowByKey = new HashMap<>();
		outputGridRowByKey = new HashMap<>();
		outputTableRowByKey = new HashMap<>();
		recommendationRowByKey = new HashMap<>();
		initialize();
	}

	/**
	 * Initialize the UI
	 */
	private void initialize() {

		UUID root = ChartUIModel.ROOT_FOLDER;
		log.tracef("%s.initialize: ROOT_FOLDER = %s",TAG,root.toString());
		update();
	}
	/**
	 * Respond to changes in the dataset configuration by re-computing the graph.
	 */
	public void update() {
		// First make sure that all are defined
		if( recmap.getDiagnoses()==null || recmap.getRecommendations()==null || recmap.getOutputs()==null) return;
		
		Dataset diagnoses = recmap.getDiagnoses();
		sourceRowCount = diagnoses.getRowCount();
		int row = 0;
		while( row<diagnoses.getRowCount()) {
			try {
				int key = Integer.parseInt(diagnoses.getValueAt(row, RecMapConstants.ID_COLUMN).toString());
				int index = addNodeTableRow(RecMapConstants.SOURCE_KIND,diagnoses.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(),key);
				diagnosisGridRowByKey.put(new Integer(key), new Integer(row));
				diagnosisTableRowByKey.put(new Integer(key), new Integer(index));
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: diagnosis ID %s for %s is not a number (%s)",TAG,
						diagnoses.getValueAt(row, RecMapConstants.ID_COLUMN).toString(),
						diagnoses.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			row++;
		}
		Dataset outputs = recmap.getOutputs();
		targetRowCount = outputs.getRowCount();
		row = 0;
		while( row<outputs.getRowCount()) {
			try {
				int key = Integer.parseInt(outputs.getValueAt(row, RecMapConstants.ID_COLUMN).toString());
				int index = addNodeTableRow(RecMapConstants.TARGET_KIND,outputs.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(),key);
				outputGridRowByKey.put(new Integer(key), new Integer(row));
				outputTableRowByKey.put(new Integer(key), new Integer(index));
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.update: output ID %s for %s is not a number (%s)",TAG,
						outputs.getValueAt(row, RecMapConstants.ID_COLUMN).toString(),
						outputs.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
			}
			row++;
		}
		Dataset recommendations = recmap.getRecommendations();
		recommendationCount = recommendations.getRowCount();
		row = 0;
		while( row<recommendations.getRowCount()) {
			int key1 = Integer.parseInt(recommendations.getValueAt(row, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString());
			Integer source = diagnosisGridRowByKey.get(new Integer(key1));
			int key2 = Integer.parseInt(recommendations.getValueAt(row, RecMapConstants.OUTPUT_ID_COLUMN).toString());
			Integer target = outputGridRowByKey.get(new Integer(key2));
			if( source!=null && target!=null ) {
				String key = String.format("%d:%d",key1,key2);
				try {
					Double dbl = Double.parseDouble(recommendations.getValueAt(row, RecMapConstants.VALUE_COLUMN).toString());
					int index = addRecNodeTableRow(recommendations.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(),
							                       source.intValue(),target.intValue(),dbl);
					recommendationRowByKey.put(key, new Integer(index));
					//log.infof("%s.update: added %d for %s to rec map",TAG,index,key);
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.update: recommended value %s for %s is not a number (%s)",TAG,
							recommendations.getValueAt(row, RecMapConstants.VALUE_COLUMN).toString(),
							recommendations.getValueAt(row, RecMapConstants.NAME_COLUMN).toString(), nfe.getMessage());
				}
			}
			else {
				log.warnf("%s.update: Recommendations %s has incorrect source %d, or target %d reference",TAG,
						recommendations.getValueAt(row, RecMapConstants.NAME).toString(),key1,key2);
			}
			row++;
		}
		

		// Create links
		row = 0;
		while( row<recommendations.getRowCount()) {
			int key1 = Integer.parseInt(recommendations.getValueAt(row, RecMapConstants.DIAGNOSIS_ID_COLUMN).toString());
			int key2 = Integer.parseInt(recommendations.getValueAt(row, RecMapConstants.OUTPUT_ID_COLUMN).toString());
			String key = String.format("%d:%d",key1,key2);
			Integer diagRow = diagnosisTableRowByKey.get(new Integer(key1));
			Integer outRow  = outputTableRowByKey.get(new Integer(key2));
			Integer recRow = recommendationRowByKey.get(key);
			if( recRow!=null) {
				if( diagRow!=null ) addEdgeTableRow(diagRow.intValue(),recRow.intValue());
				if( outRow!=null )  addEdgeTableRow(recRow.intValue(),outRow.intValue());
			}
			else {
				log.warnf("%s.update: Recommendation not found for key %s",TAG,key);
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
		nodes.setInt(row,RecMapConstants.KIND,kind); 
		nodes.setString(row,RecMapConstants.NAME,name);
		nodes.setInt(row,RecMapConstants.ID,key);
		nodes.setInt(row,RecMapConstants.ROW,row);
		nodes.setDouble(row,RecMapConstants.VALUE,0.0);
		return row;
	}
	// Add a row to the nodes list
	// @return the number of the newly added row
	private int addRecNodeTableRow(String name,int source,int target,Double value) {
		int row = nodes.getRowCount();
		log.infof("%s.addRecNodeTableRow: %d = %s (%d->%d)", TAG,row,name,source,target);
		nodes.addRow();
		nodes.setInt(row,RecMapConstants.KIND,RecMapConstants.LINK_KIND); 
		nodes.setString(row,RecMapConstants.NAME,name);
		nodes.setInt(row,RecMapConstants.ROW,row);
		nodes.setInt(row,RecMapConstants.SOURCEROW,source);
		nodes.setInt(row,RecMapConstants.TARGETROW,target);
		nodes.setDouble(row,RecMapConstants.VALUE,value);
		return row;
	}
	
}
