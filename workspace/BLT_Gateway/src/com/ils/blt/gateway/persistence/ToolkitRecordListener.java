/**
 *   (c) 2015-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.persistence;

import java.util.List;

import com.ils.blt.common.DiagramState;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.common.persistence.BasicToolkitProjectRecordListener;
import com.ils.common.persistence.ToolkitProjectRecord;
import com.ils.common.persistence.ToolkitProperties;
import com.inductiveautomation.ignition.gateway.localdb.persistence.IRecordListener;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;



/**
 * Respond to changes in our toolkit properties in the HSQL persistent database.
 * For documentation relating to the SimpleORM data model:
 * See: http://simpleorm.org/sorm/whitepaper.html
 */
public class ToolkitRecordListener extends BasicToolkitProjectRecordListener implements IRecordListener<ToolkitProjectRecord> {
	private final static String CLSS = "ToolkitRecordListener";

	
	public ToolkitRecordListener(GatewayContext ctx) {
		super(ctx);
	}

	@Override
	public void recordUpdated(ToolkitProjectRecord rec) {
		log.debugf("%s.recordUpdated: %s:%s = %s",CLSS,rec.getProject(),rec.getName(),rec.getValue());
		if( ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER.equalsIgnoreCase(rec.getName())) productionProviderUpdated(rec.getProject(),rec.getValue());
		else if( ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER.equalsIgnoreCase(rec.getName())) isolationProviderUpdated(rec.getProject(),rec.getValue());
		
	}
	
	public void isolationProviderUpdated(String projectName,String providerName) {
		setTagProvider(projectName,DiagramState.ISOLATED,providerName);
	}
	
	public void productionProviderUpdated(String projectName,String providerName) {
		setTagProvider(projectName,DiagramState.ACTIVE,providerName);
	}
	
	// Set the tag providers for all properties in diagrams that match
	// the specified state.
	private void setTagProvider(String projectName,DiagramState state,String providerName) {
		ModelManager mm = BlockExecutionController.getInstance().getDelegate();
		List<ProcessDiagram> diagrams = mm.getDiagrams();
		for(ProcessDiagram diagram:diagrams) {
			if(diagram.getProjectName().equals(projectName) && diagram.getState().equals(state)) {
				diagram.stopSubscriptions();
				diagram.updatePropertyProviders(providerName);
				diagram.startSubscriptions(state);
			}
		}
	}
	
}
