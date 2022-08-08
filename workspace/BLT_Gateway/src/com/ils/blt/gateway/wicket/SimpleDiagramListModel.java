package com.ils.blt.gateway.wicket;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.wicket.model.LoadableDetachableModel;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessDiagram;


public class SimpleDiagramListModel extends LoadableDetachableModel<List<ProcessDiagram>> {
	private static final long serialVersionUID = 3174385295574411202L;

	public SimpleDiagramListModel() {	
	}


	/**
	 * @return a sorted list of diagrams.
	 */
	@Override
	protected List<ProcessDiagram> load() {
		ModelManager modelManager = BlockExecutionController.getInstance().getDelegate();
		List<ProcessDiagram> diagrams = modelManager.getDiagrams(); 

		Collections.sort(diagrams, new Comparator<ProcessDiagram>() {
			@Override
			public int compare(ProcessDiagram a, ProcessDiagram b) {
				if(a.getProjectName().equalsIgnoreCase(b.getProjectName())) {
					return a.getResourceId().getFolderPath().compareTo(b.getResourceId().getFolderPath());
				}
				else {
					return a.getProjectName().compareTo(b.getProjectName());
				}
			}
		});
		return diagrams;
	}
}
