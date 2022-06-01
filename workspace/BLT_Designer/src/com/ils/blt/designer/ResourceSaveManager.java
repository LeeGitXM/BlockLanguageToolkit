/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.io.IOException;
import java.util.Map;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.ignition.designer.project.DesignableProject;


/**
 * Search the descendants of the specified node, looking for open diagrams.
 * Close them and save them along with any dirty nodes to the project and gateway.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Do not re-execute the same instance.
 *
 */
public class ResourceSaveManager {
	private final String CLSS = "ResourceSaveManager";
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	private final ObjectMapper mapper;
	private final ApplicationRequestHandler requestHandler;
	private final NodeStatusManager statusManager;
	
	public ResourceSaveManager(DesignerContext ctx,DiagramWorkspace wksp) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.context = ctx;
		this.workspace = wksp;
		this.requestHandler = new ApplicationRequestHandler();
		this.statusManager = NodeStatusManager.getInstance();
		this.mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
	}
	

	/**
	 * Traverse the entire node hierarchy looking for diagrams that need saving.
	 * When found, serialize into the project resource. This is in anticipation
	 * of a top-level save. This method is called from the designer hook and 
	 * runs in the foreground.
	 */
	public void execute(SaveContext saveContext) {
		if( DEBUG ) log.infof("%s.execute()", CLSS);
		saveModifiedResources(saveContext);
	}

	/**
	 *  Recursively descend the node tree, looking for resources in need of saving.
	 *  These are the cases:
	 *  1) Any diagram that is "dirty".
	 *  2) Diagrams that are in a different state than the gateway version
	 *  3) A folder or diagram that has been renamed
	 *  4) Either a folder or diagram that is new (i.e. never edited)
	 */
	private void saveModifiedResources(SaveContext saveContext) {
		int count = statusManager.getModificationCount();
		if( count==0 ) count = 1;
		int index = 0;
		DesignableProject project = context.getProject();
		Map<ProjectResourceId,ProjectResource> map = context.getProject().getAllResources();
		for(ProjectResourceId resid:map.keySet()) {
			if( resid.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) && statusManager.isModified(resid)) {
				saveContext.setProgress(index++/count);
				ProjectResource res = map.get(resid);  // This is the "clean" copy
				ResourcePath respath = resid.getResourcePath();
				StringPath stringPath = respath.getPath();
				String name = stringPath.getLastPathComponent();
				
				ProjectResourceBuilder builder = res.toBuilder();
				builder.clearData();
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				if( res.isFolder()) {      // Folder
					builder.setFolder(true);
				}
				else {                     // Diagram
					builder.setFolder(false);
					// If the resource is open in the workspace and dirty, move it to the status manager
					BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourcePath());
					if( tab!=null ) {
						ProcessDiagramView tabView = (ProcessDiagramView)tab.getModel();
						if( DEBUG ) log.infof("%s.saveModifiedResources, %s (%s)", CLSS, tabView.getName(), (tabView.isChanged()?"CHANGED":"UNCHANGED"));
						if( tabView.isChanged() ) {
							statusManager.setPendingView(resid, tabView);
							tabView.registerChangeListeners();     // The diagram may include new components
							if( DEBUG ) log.infof("%s.saveModifiedResource: Saving modified %s...", CLSS, tabView.getName());
						}
					}
					ProcessDiagramView view = statusManager.getPendingView(resid);
					if( view==null) {
						// Serialize from the resource
						byte[] bytes = res.getData();
						try {
							SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
							if( sd!=null ) {
								view = new ProcessDiagramView(context,resid,sd);
							}
						}
						catch(JsonParseException jpe) {
							saveContext.abort(jpe);
							log.warnf("%s.saveModifiedResource: Parse exception saving %s...(%s)", CLSS, res.getResourceName(),jpe.getLocalizedMessage());
						}
						catch(JsonMappingException jme) {
							saveContext.abort(jme);
							log.warnf("%s.saveModifiedResource: Mapping exception saving %s...(%s)", CLSS, res.getResourceName(),jme.getLocalizedMessage());
						}
						catch(IOException ioe) {
							saveContext.abort(ioe);
							log.warnf("%s.saveModifiedResource: IO exception saving %s...(%s)", CLSS, res.getResourceName(),ioe.getLocalizedMessage());
						}
					}
					DiagramState state = statusManager.getPendingState(resid);
					if( state!=null) view.setState(state);
					else view.setState(requestHandler.getDiagramState(resid));
					String pendingName = statusManager.getPendingName(resid);
					if( pendingName!=null && !pendingName.equalsIgnoreCase(name)) {
						stringPath = StringPath.extend(stringPath.getParentPath(),pendingName);
						respath = new ResourcePath(BLTProperties.DIAGRAM_RESOURCE_TYPE,stringPath);
						builder.setResourcePath(respath);
					}
					SerializableDiagram sd = view.createSerializableRepresentation();
					builder.putData(sd.serialize());
					res = builder.build();
					project.createOrModify(res);	
				}
				statusManager.clearChangeMarkers(resid);
				requestHandler.triggerStatusNotifications(context.getProjectName());
			}
		}
	}
}
