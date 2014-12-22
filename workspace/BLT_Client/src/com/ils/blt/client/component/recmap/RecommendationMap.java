/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import java.awt.BorderLayout;

import com.ils.blt.client.component.PrefuseViewerComponent;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BasicDataset;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.Dataset;

/**
 *  Use Prefuse to display a map of Recommendations to Outputs. The map
 *  allows interaction to update the recommendation priorities. The core data
 *  structure consists of three datasets:
 *  1) A list of outputs (name)
 *  2) A single column list of diagnoses (name)
 *  3) A three column list of recommendations: output(name), recommendation, final diagnosis(name).
 *     The recommendation factor is editable.
 */
public class RecommendationMap extends PrefuseViewerComponent {
	private static final long serialVersionUID = 5508313516136446100L;
	private static final String TAG = "RecommendationMap";
	private static String PREFIX = BLTProperties.CUSTOM_PREFIX;              // For bundle identification
	
	private RecMapDataModel model = null;
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
	
	// We need getters/setters for all the bound properties
	public Dataset getDiagnoses() {return diagnoses;}

	public void setDiagnoses(Dataset diagnoses) {
		this.diagnoses = diagnoses;
		updateChartView();
	}
	public Dataset getOutputs() {return outputs;}
	public void setOutputs(Dataset outputs) {
		this.outputs = outputs;
		updateChartView();
	}
	
	public RecMapDataModel getModel() { return model; }
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
		this.recommendations = new BasicDataset(recs);
		updateChartView();
	}
	public void updateRecommendations(int row,String value) {
		int col = recommendations.getColumnIndex(RecMapConstants.VALUE_COLUMN);
		if( col>=0 ) {
			recommendations.setValueAt(row, col, value);
		}
	}
}