/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.border.Border;

import com.ils.blt.client.component.PrefuseViewerComponent;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BasicDataset;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.Dataset;
import com.inductiveautomation.ignition.common.script.ScriptManager;

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
	private static final Dimension MIN_SIZE = new Dimension(40,40);
	private static String PREFIX = BLTProperties.CUSTOM_PREFIX;              // For bundle identification
	private RecMapDataModel model = null;
	private RecMapView view = null;
	private BasicDataset connections = null;
	private BasicDataset outputs = null;
	private BasicDataset recommendations = null;
	private BasicDataset diagnoses = null;

	public RecommendationMap() {
		setName(BundleUtil.get().getString(PREFIX+".RecommendationMap.Name"));
		this.setOpaque(true);
		updateChartView();
	}

	private void updateChartView() {
		if( isConfigured() ) {
			removeAll();
			invalidate();
			log.infof("%s.updateChartView: Creating RecommendationMapView ..%d x %d.",TAG,getWidth(),getHeight());
			view = createMapView();
			add(view,BorderLayout.CENTER);
			view.setSize(getWidth(), getHeight());
			// Resize and center
			view.zoomToFit();
			//validate();
			//repaint();
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
	public void setDiagnoses(Dataset newDiagnoses) {
		firePropertyChange(RecMapConstants.DIAGNOSES_PROPERTY,this.diagnoses,newDiagnoses);
		this.diagnoses = new BasicDataset(newDiagnoses);
		updateChartView();
	}
	public Dataset getOutputs() {return outputs;}
	public void setOutputs(Dataset newOutputs) {
		firePropertyChange(RecMapConstants.OUTPUTS_PROPERTY,this.outputs,newOutputs);
		this.outputs = new BasicDataset(newOutputs);
		updateChartView();
	}
	
	public Dataset getRecommendations() {return recommendations;}
	public void setRecommendations(Dataset newRecommendations) {
		firePropertyChange(RecMapConstants.RECOMMENDATIONS_PROPERTY,this.recommendations,newRecommendations);
		this.recommendations = new BasicDataset(newRecommendations);
		updateChartView();
	}
	
	// It is important not to save the script manager, but to get it from the context each time.
	public ScriptManager getScriptManager() { return context.getScriptManager(); }
	
	@Override
	public Dimension getSize() {
		
		Dimension dim = super.getSize();
		if( view!=null ) dim = view.getSize();
		if(dim==null || dim.getWidth()<MIN_SIZE.getWidth()) {
			dim = MIN_SIZE;
		}
		return dim;
	}
	
	// TODO: Check dataset columns
	public boolean isConfigured() {
		boolean valid = false;
		if( connections!=null && connections.getRowCount()>0 &&
			diagnoses!=null && diagnoses.getRowCount()>0 &&
			recommendations!=null &&
			outputs!=null && outputs.getRowCount()>0 	) {
			valid = true;
		}
		return valid;
	}

	public Border getBorder() { return (view==null? null:view.getBorder()); } 
	public void setBorder(Border bdr) { if( view!=null ) view.setBorder(bdr); } 
	public void setSize(Dimension sz) { if( view!=null ) view.setSize(sz); }

	public void updateDiagnosis(int row,String value) {
		int col = diagnoses.getColumnIndex(RecMapConstants.MULTIPLIER);
		if( col>=0 ) {
			Dataset old = new BasicDataset(diagnoses);
			diagnoses.setValueAt(row, col, value);
			firePropertyChange(RecMapConstants.DIAGNOSES_PROPERTY,old,this.diagnoses);
			log.infof("%s.updateDiagnosis: Fired property change on %s",TAG,RecMapConstants.DIAGNOSES_PROPERTY);
		}
	}
}
