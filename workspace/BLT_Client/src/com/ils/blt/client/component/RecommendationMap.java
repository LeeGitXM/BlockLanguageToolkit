/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component;

import java.awt.BorderLayout;

import com.ils.blt.common.BLTProperties;
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
	
	private Dataset outputs = null;
	private Dataset recommendations = null;
	private Dataset diagnoses = null;

	public RecommendationMap() {
		
		setName(BundleUtil.get().getString(PREFIX+".RecommendationMap.Name"));
		this.setOpaque(true);
		this.setBorder(border);
		updateChartView();
	}

	private void updateChartView() {
		removeAll();
		invalidate();
		log.infof("%s.update: Creating RecommendationMapView ...",TAG);
		RecommendationMapView view = createMapView();
		view.setSize(getWidth(), getHeight());
		add(view,BorderLayout.CENTER);
		validate();
		log.infof("%s.update: Created RecommendationMapView ...",TAG);
		repaint();
	}
	
	private RecommendationMapView createMapView() {
		log.infof("%s.createMapView: New view ....",TAG);
		RecommendationMapDataModel model = new RecommendationMapDataModel(context,this);
		return new RecommendationMapView(model,RecommendationConstants.NAME,model.getSourceRowCount(),
										 model.getRecommendationCount(),model.getTargetRowCount(),
										 RecommendationConstants.KIND,RecommendationConstants.SOURCEROW,
										 RecommendationConstants.TARGETROW);
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
	public Dataset getRecommendations() {return recommendations;}
	public void setRecommendations(Dataset recommendations) {this.recommendations = recommendations;updateChartView();}



}
