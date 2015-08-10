package com.ils.blt.designer.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.gui.progress.TaskProgressListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectAggregator;
import com.inductiveautomation.ignition.designer.findreplace.SearchProvider;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

public class BLTSearchProvider implements SearchProvider {
	private final String TAG = "BLTSearchProvider";
	private final LoggerEx log;
	private final DesignerContext context;
	
	public BLTSearchProvider(DesignerContext ctx ) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	@Override
	public List<Object> getCategories() {
		List<Object> cts = new ArrayList<>();
		cts.add("Application");
		cts.add("Family");
		cts.add("Diagram");
		cts.add("Block");
		return cts;
	}

	@Override
	public String getName() {
		return "Diagnostic Toolkit";
	}

	@Override
	public boolean hasSelectableObjects() {
		return true;
	}

	@Override
	public void notifySearchClosed() {
		// Close any resource edit locks here.
		
	}

	@Override
	public Iterator<SearchObject> retrieveSearchableObjects(Collection<Object> selectedCategories, List<Object> arg1,TaskProgressListener progress) {
		SearchObjectAggregator agg = new SearchObjectAggregator(progress);
		List<ProjectResource> resources = null;
		if( selectedCategories.contains("Diagram") || selectedCategories.contains("Block") ) {
			boolean searchDiagrams = selectedCategories.contains("Diagram");
			boolean searchBlocks   = selectedCategories.contains("Block");
			resources = context.getProject().getResourcesOfType(BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE);
			for(ProjectResource res:resources) {
				log.infof("%s.retrieveSearchableObjects resId = %d",TAG,res.getResourceId());
				agg.add(new DiagramSearchCursor(context,res.getResourceId(),searchDiagrams,searchBlocks));
			}
		}
		if( selectedCategories.contains("Application") ) {
			resources = context.getProject().getResourcesOfType(BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE);
			for(ProjectResource res:resources) {
				log.infof("%s.retrieveSearchableObjects resId = %d",TAG,res.getResourceId());
				agg.add(new ApplicationSearchCursor(context,res.getResourceId()));
			}
		}
		
		if( selectedCategories.contains("Family") ) {
			resources = context.getProject().getResourcesOfType(BLTProperties.MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE);
			for(ProjectResource res:resources) {
				log.infof("%s.retrieveSearchableObjects resId = %d",TAG,res.getResourceId());
				agg.add(new FamilySearchCursor(context,res.getResourceId()));
			}
		}
		return agg;
	}

	@Override
	public void selectObjects(SelectedObjectsHandler arg0) {
		log.infof("%s.selectObjects",TAG);
		
	}

	@Override
	public String selectedObjectsToString(List<Object> arg0) {
		log.infof("%s.selectedObjectsToString",TAG);
		return null;
	}
}
