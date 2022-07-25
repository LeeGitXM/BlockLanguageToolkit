package com.ils.blt.designer.search;

import java.io.IOException;
import java.util.Iterator;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.BlockAttributeView;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

public class DiagramSearchCursor extends SearchObjectCursor {
	private final String CLSS = "DiagramSearchCursor";
	private final DesignerContext context;
	private ProcessDiagramView diagram; 
	private final LoggerEx log;
	private final ProjectResource resource;
	private final boolean searchDiagrams;
	private final boolean searchBlocks;
	private final int searchKey;
	private int index = 0;
	private Iterator<? extends Block> blockWalker; 
	
	public DiagramSearchCursor(DesignerContext ctx,ProjectResource res,int key) {
		this.context = ctx;
		this.resource = res;
		this.searchKey = key;
		this.searchDiagrams = (key&BLTSearchProvider.SEARCH_DIAGRAM)!=0;
		this.searchBlocks = (key & (BLTSearchProvider.SEARCH_BLOCK +BLTSearchProvider.SEARCH_PROPERTY) )!=0;
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.index = 0;
		this.blockWalker = null;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		// Deserialize here - first time through only - return next block cursor
		if( index==0 ) {
			diagram = deserializeResource(resource);
			blockWalker = diagram.getBlocks().iterator();
		}
		
		if( index==0 && searchDiagrams ) {
			so = new DiagramNameSearchObject(context,diagram);
			log.infof("%s.next: Searching %s",CLSS,diagram.getDiagramName());
		}
		else if( searchBlocks ) {
			if( blockWalker.hasNext() ) {
				ProcessBlockView view = (ProcessBlockView)blockWalker.next();
				while( view instanceof BlockAttributeView ) {
					view = null;
					if( blockWalker.hasNext()) {
						view = (ProcessBlockView)blockWalker.next();
					}
				}
				if( view !=null ) so = new BlockSearchCursor(context,diagram,view,searchKey);
			}
		}
		index++;
		return so;
	}
	private ProcessDiagramView deserializeResource(ProjectResource res) {
		String json = new String(res.getData());
		//log.debugf("%s: open - diagram = %s",CLSS,json);
		SerializableDiagram sd = null;
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
		try {
			sd = mapper.readValue(json,SerializableDiagram.class);
			// Synchronize names as the resource may have been re-named since it was serialized
			sd.setName(res.getResourceName());
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
		} 
		catch (JsonMappingException jme) {
			log.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
		} 
		catch (IOException ioe) {
			log.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
		}
		ProcessDiagramView dgm = new ProcessDiagramView(context,res.getResourceId(),sd);
		return dgm;
	}
}
