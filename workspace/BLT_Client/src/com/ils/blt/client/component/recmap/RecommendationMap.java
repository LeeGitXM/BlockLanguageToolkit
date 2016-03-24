/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import java.awt.BorderLayout;

import com.ils.blt.client.component.PrefuseViewerComponent;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BasicDataset;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.Dataset;

/**
 *  Use Prefuse to display a map of Diagnoses to Outputs and intervening Recommendations.
 *  Block attribute values are all derived from the input datasets - there are no calculations
 *  performed. The core data structure consists of four datasets:
 *  1) A list of quant outputs: id, name, currentSetpoint, finalSetpoint, recommendation, target
 *  2) A list of diagnoses: id, name, problem, multiplier 
 *  3) A list of connections between final diagnoses and outputs: dignosisId, outputId, active
 *  4) A list of recommendations: diagnosisId, outputId, auto.
 *     The diagnosis multiplier is editable.
 */
public class RecommendationMap extends PrefuseViewerComponent {
	private static final long serialVersionUID = 5508313516136446100L;
	private static final String TAG = "RecommendationMap";
	private static String PREFIX = BLTProperties.CUSTOM_PREFIX;              // For bundle identification
	private RecMapDataModel model = null;
	private BasicDataset connections = null;
	private Dataset outputs = null;
	private BasicDataset recommendations = null;
	private Dataset diagnoses = null;

	public RecommendationMap() {
		setName(BundleUtil.get().getString(PREFIX+".RecommendationMap.Name"));
		this.setOpaque(true);
		this.setBorder(border);
		updateChartView();
	}

	private void updateChartView() {
		if( configured() ) {
			removeAll();
			invalidate();
			log.infof("%s.update: Creating RecommendationMapView ..%d x %d.",TAG,getWidth(),getHeight());
			RecMapView view = createMapView();
			view.setSize(getWidth(), getHeight());
			add(view,BorderLayout.CENTER);
			validate();
			repaint();
			log.infof("%s.update: Created RecommendationMapView ...",TAG);
		}
	}
	
	private RecMapView createMapView() {
		log.infof("%s.createMapView: New view ....",TAG);
		model = new RecMapDataModel(context,this);
		return new RecMapView(this);
	}
	public RecMapDataModel getModel() { return model; }
	
	// We need getters/setters for all the bound properties
	public Dataset getConnections() {return connections;}
	public void setConnections(Dataset newConnections) {
		firePropertyChange(RecMapConstants.CONNECTIONS_PROPERTY,this.connections,newConnections);
		this.connections = new BasicDataset(newConnections);
		updateChartView();
	}
	
	public Dataset getDiagnoses() {return diagnoses;}
	public void setDiagnoses(Dataset diagnoses) {
		this.diagnoses = diagnoses;
		updateChartView();
	}
	public Dataset getOutputs() {return outputs;}
	public void setOutputs(Dataset newOutputs) {
		updateChartView();
		
		this.outputs = newOutputs;
	}
	
	
	public Dataset getRecommendations() {return recommendations;}
	
	
	// TODO: Check dataset columns
	private boolean configured() {
		boolean valid = false;
		if( diagnoses!=null && diagnoses.getRowCount()>0 &&
			recommendations!=null && recommendations.getRowCount()>0 &&
			outputs!=null && outputs.getRowCount()>0 	) {
			valid = true;
		}
		return valid;
	}
	public void setRecommendations(Dataset recs) {
		firePropertyChange(RecMapConstants.RECOMMENDATIONS_PROPERTY,this.recommendations,recs);
		this.recommendations = new BasicDataset(recs);
		updateChartView();
	}
	public void updateRecommendations(int row,String value) {
		int col = recommendations.getColumnIndex(RecMapConstants.VALUE_COLUMN);
		if( col>=0 ) {
			Dataset old = new BasicDataset(recommendations);
			recommendations.setValueAt(row, col, value);
			firePropertyChange(RecMapConstants.RECOMMENDATIONS_PROPERTY,old,this.recommendations);
			log.infof("%s.updateRecommendations: Fired property change on %s",TAG,RecMapConstants.RECOMMENDATIONS_PROPERTY);
		}
	}
}
