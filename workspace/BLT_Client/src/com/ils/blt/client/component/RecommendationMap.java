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
 *  1) A single column list of outputs
 *  2) A single column list of diagnoses
 *  3) A three column list of recommendations: output, recommendation, final diagnosis.
 *     The factor is editable.
 */
public class RecommendationMap extends PrefuseViewerComponent {
	private static final long serialVersionUID = 5508313516136446100L;
	private static final String TAG = "RecommendationMap";
	private static String PREFIX = BLTProperties.CUSTOM_PREFIX;              // For bundle identification
	
	// These are the property names for the bean info es";
	public static final String DIAGNOSES_PROPERTY          = "diagnoses";
	public static final String OUTPUTS_PROPERTY            = "outputs";
	public static final String RECOMMENDATIONS_PROPERTY    = "recommendations";
	
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
		RecommendationMapView view = createMapView();
		add(view,BorderLayout.CENTER);
		validate();
		setVisible(true);
	}
	
	private RecommendationMapView createMapView() {
		log.infof("%s.createMapView: New view ....",TAG);
		RecommendationMapDataModel model = new RecommendationMapDataModel(context);
		return new RecommendationMapView(model,RecommendationMapDataModel.NAME);
	}
	
	// We need getters/setters for all the bound properties
	public Dataset getDiagnoses() {return diagnoses;}

	public void setDiagnoses(Dataset diagnoses) {
		this.diagnoses = diagnoses;
	}
	public Dataset getOutputs() {return outputs;}
	public void setOutputs(Dataset outputs) {
		this.outputs = outputs;
	}
	public Dataset getRecommendations() {return recommendations;}
	public void setRecommendations(Dataset recommendations) {this.recommendations = recommendations;}



}
