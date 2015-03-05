package com.ils.blt.gateway;

import java.util.List;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessDiagram;


public class StatusData {
	private final ModelManager modelManager;
	
	public StatusData() {
		modelManager = BlockExecutionController.getInstance().getDelegate();
	}
	
	public List<ProcessDiagram> getDiagrams() { return modelManager.getDiagrams(); }
}
