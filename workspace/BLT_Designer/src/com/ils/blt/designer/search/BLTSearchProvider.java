package com.ils.blt.designer.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

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
		cts.add("Block");
		return cts;
	}

	@Override
	public String getName() {
		return "Diagnostic Toolkit";
	}

	@Override
	public boolean hasSelectableObjects() {
		return false;
	}

	@Override
	public void notifySearchClosed() {
		// Close any resource edit locks here.
		
	}

	@Override
	public Iterator<SearchObject> retrieveSearchableObjects(
			Collection<Object> selectedCategories, List<Object> arg1,
			TaskProgressListener progress) {
		SearchObjectAggregator agg = new SearchObjectAggregator(progress);
		List<ProjectResource> resources = context.getProject().getResourcesOfType("block", "blt.diagram");
		for(ProjectResource res:resources) {
			log.infof("%s.retrieveSearchableObjects resId = %d",TAG,res.getResourceId());
			agg.add(new DiagramSearchCursor(context,res.getResourceId()));
		}
		return agg;
	}

	@Override
	public void selectObjects(SelectedObjectsHandler arg0) {
		// ignore
		
	}

	@Override
	public String selectedObjectsToString(List<Object> arg0) {
		// ignore
		return null;
	}
}
