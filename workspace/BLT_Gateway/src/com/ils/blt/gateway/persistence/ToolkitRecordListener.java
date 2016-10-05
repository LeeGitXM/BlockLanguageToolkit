/**
 *   (c) 2015-2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.persistence;

import java.util.List;

import com.ils.blt.common.DiagramState;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.common.persistence.BasicToolkitRecordListener;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.persistence.ToolkitRecord;
import com.inductiveautomation.ignition.gateway.localdb.persistence.IRecordListener;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;



/**
 * Respond to changes in our toolkit properties in the HSQL persistent database.
 * For documentation relating to the SimpleORM data model:
 * See: http://simpleorm.org/sorm/whitepaper.html
 */
public class ToolkitRecordListener extends BasicToolkitRecordListener implements IRecordListener<ToolkitRecord> {
	private final static String TAG = "ToolkitRecordListener";

	
	public ToolkitRecordListener(GatewayContext ctx) {
		super(ctx);
	}

	@Override
	public void recordUpdated(ToolkitRecord rec) {
		log.debugf("%s.recordUpdated: %s = %s",TAG,rec.getName(),rec.getValue());
		if( ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER.equalsIgnoreCase(rec.getName())) productionProviderUpdated(rec.getValue());
		else if( ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER.equalsIgnoreCase(rec.getName())) isolationProviderUpdated(rec.getValue());
		
	}
	
	public void isolationProviderUpdated(String providerName) {
		setTagProvider(DiagramState.ISOLATED,providerName);
	}
	
	public void productionProviderUpdated(String providerName) {
		setTagProvider(DiagramState.ACTIVE,providerName);
	}
	
	// Set the tag providers for all properties in diagrams that match
	// the specified state.
	private void setTagProvider(DiagramState state,String name) {
		ModelManager mm = BlockExecutionController.getInstance().getDelegate();
		List<ProcessDiagram> diagrams = mm.getDiagrams();
		for(ProcessDiagram diagram:diagrams) {
			if(diagram.getState().equals(state)) {
				diagram.stopSubscriptions();
				diagram.updatePropertyProviders(name);
				diagram.startSubscriptions(state);
			}
		}
	}
	
}
