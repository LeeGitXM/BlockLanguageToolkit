/**
 *   (c) 2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.ProjectDiff;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.DesignerContextImpl;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.DesignerProjectTreeImpl;
import com.inductiveautomation.ignition.designer.project.ProjectChange;


/**
 * Push changes to the Gateway for the the single specified diagram. We assume it is dirty.
 */
public class DiagramPushTask implements Runnable {
	private static final String CLSS = "DiagramPushTask";
	private static final LoggerEx log = LogUtil.getLogger(DiagramPushTask.class.getPackage().getName());
	private static final boolean DEBUG = true;
	private DesignerContextImpl context = null;
	private final ProcessDiagramView view;
	private ProjectResource resource;
	
	/**
	 * This constructor is used for diagram resources.
	 * @param pr
	 */
	public DiagramPushTask(DesignerContext ctx,ProjectResource pr,ProcessDiagramView pdv) {
		this.context = (DesignerContextImpl)ctx;
		this.resource = pr;
		this.view = pdv;
	}
	
	/**
	 *  Now save the resource, as it is.
	 */
	@Override
	public void run() {
		ProjectResourceId resid = resource.getResourceId();
		ResourcePath respath = resid.getResourcePath();
		if( respath.getParent()==null ) return;  // Ignore "system" resources

		NodeStatusManager statusManager = NodeStatusManager.getInstance();
		String pendingName = statusManager.getPendingName(resid);
		ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();

		ProjectResourceBuilder builder = resource.toBuilder();
		builder.clearData();

		builder.setFolder(false);
		view.setChanged(false);
		//view.setState(statusManager.getPendingState(resid));
		byte[] bytes = view.createSerializableRepresentation().serialize();
		builder.putData(bytes);

		StringPath sp = respath.getPath();
		String name = sp.getLastPathComponent();
		if(!name.equalsIgnoreCase(pendingName)) {
			sp = StringPath.extend(sp.getParentPath(),pendingName);
			respath = new ResourcePath(BLTProperties.DIAGRAM_RESOURCE_TYPE,sp);
			builder.setResourcePath(respath);
		}
		resource = builder.build();
		DesignerProjectTreeImpl project = context.getProject();
		
		try {
			ChangeOperation.ResourceChangeOperation co = ChangeOperation.ResourceChangeOperation.newModifyOp(resource,resource.getResourceSignature());
			List<ChangeOperation> ops = new ArrayList<>();
			ops.add(co);
			ProjectDiff diff = ProjectDiff.AbsoluteDiff.newAbsoluteDiff(resid.getProjectName(), ops);
			List<ProjectDiff> diffs = new ArrayList<>();
			diffs.add(diff);
			ProjectChange change = new ProjectChange(diffs);
			change.putChoice(co, ProjectChange.ConflictChoice.useLocal());
			
			project.applyChange(change);
			project.createOrModify(resource);
			project.notifyPushComplete(ops);
			requestHandler.triggerStatusNotifications(context.getProjectName());
			statusManager.clearChangeMarkers(resource.getResourceId());
			
			List<ProjectResource> resources = new ArrayList<>();
			for(ProjectResource res:project.getAllResources().values() ) {  // Includes from parent projects, if any
				if( res.getProjectName().equals(project.getName())) resources.add(res);
				
			}
		}
		catch(Exception ex) {
			log.warn(String.format("%s.run: Exception modifying resource %s:%s (%s)",CLSS,resource.getResourceId().getProjectName(),
					resource.getResourceId().getResourcePath().getPath().toString(),ex.getMessage()),ex);
		}

		if(DEBUG) log.infof("%s.run(): complete",CLSS);
	}
}
