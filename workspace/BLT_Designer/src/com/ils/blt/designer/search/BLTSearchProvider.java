package com.ils.blt.designer.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.ils.blt.common.BLTProperties;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.gui.progress.TaskProgressListener;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectAggregator;
import com.inductiveautomation.ignition.designer.findreplace.SearchProvider;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

public class BLTSearchProvider implements SearchProvider {
	private final String CLSS = "SAISearchProvider";
	public final static int SEARCH_APPLICATION = 1;
	public final static int SEARCH_FAMILY = 2;
	public final static int SEARCH_DIAGRAM = 4;
	public final static int SEARCH_BLOCK = 8;
	public final static int SEARCH_PROPERTY = 16;
	private final ILSLogger log;
	private final DesignerContext context;
	
	public BLTSearchProvider(DesignerContext ctx ) {
		this.context = ctx;
		this.log = LogMaker.getLogger(this);
	}

	@Override
	public List<Object> getCategories() {
		List<Object> cts = new ArrayList<>();
		cts.add("Application");
		cts.add("Family");
		cts.add("Diagram");
		cts.add("Block");
		cts.add("Property");
		return cts;
	}

	@Override
	public String getName() {
		return "Symbolic AI";
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
		int searchKey = 0;
		
		if( selectedCategories.contains("Application") ) searchKey += SEARCH_APPLICATION;
		if( selectedCategories.contains("Family") ) searchKey += SEARCH_FAMILY;
		if( selectedCategories.contains("Diagram") ) searchKey += SEARCH_DIAGRAM;
		if( selectedCategories.contains("Block") ) searchKey += SEARCH_BLOCK;
		if( selectedCategories.contains("Property") ) searchKey += SEARCH_PROPERTY;
		
		if( selectedCategories.contains("Diagram") || selectedCategories.contains("Block") ) {
			resources = context.getProject().getResourcesOfType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
			for(ProjectResource res:resources) {
				log.infof("%s.retrieveSearchableObjects resId = %d",CLSS,res.getResourceId());
				agg.add(new DiagramSearchCursor(context,res,searchKey));
			}
		}
		if( selectedCategories.contains("Application") ) {
			resources = context.getProject().getResourcesOfType(BLTProperties.APPLICATION_RESOURCE_TYPE);
			for(ProjectResource res:resources) {
				log.infof("%s.retrieveSearchableObjects resId = %d",CLSS,res.getResourceId());
				agg.add(new ApplicationSearchCursor(context,res));
			}
		}
		
		if( selectedCategories.contains("Family") ) {
			resources = context.getProject().getResourcesOfType(BLTProperties.FAMILY_RESOURCE_TYPE);
			for(ProjectResource res:resources) {
				log.infof("%s.retrieveSearchableObjects resId = %d",CLSS,res.getResourceId());
				agg.add(new FamilySearchCursor(context,res));
			}
		}
		return agg;
	}

	@Override
	public void selectObjects(SelectedObjectsHandler arg0) {
		log.infof("%s.selectObjects",CLSS);
		
	}

	@Override
	public String selectedObjectsToString(List<Object> arg0) {
		log.infof("%s.selectedObjectsToString",CLSS);
		return null;
	}
	
}
