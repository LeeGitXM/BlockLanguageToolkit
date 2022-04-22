package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourceNamingException;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.DesignableProject;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Create a new resource and add it to the project.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Do not re-execute the same instance.
 * 
 * @author chuckc
 *
 */
public class ResourceCreateManager implements Runnable {
	private static final String CLSS = "ResourceCreateManager";
	private final LoggerEx log;
	private static DesignerContext context = null;
	private final ProjectResource resource;
	private final String resourceName;

	/**
	 * Use this version of the constructor to create or modify a specified resource.
	 * @param pr
	 */
	public ResourceCreateManager(String parent,String nam) {
		this.resourceName = nam;
		this.resource = createFolderResource(parent,nam);
		this.log = LogUtil.getLogger(getClass().getPackageName());
	}
	
	public ResourceCreateManager(String parent,String nam,byte[]json) {
		this.resourceName = nam;
		this.resource = createDiagramResource(parent,resourceName,json);
		this.log = LogUtil.getLogger(getClass().getPackageName());
	}
	
	// Create a folder resource and add it to the project.
	private ProjectResource createFolderResource(String parent,String name) {
		ProjectResourceBuilder builder = ProjectResource.newBuilder();
		ResourceType rtype = BLTProperties.DIAGRAM_RESOURCE_TYPE;   // blt.diagram
		ProjectResourceId resourceId = new ProjectResourceId(context.getProjectName(),rtype,null);
		builder.setResourceId(resourceId);
		ResourcePath path = new ResourcePath(rtype,parent+"/"+name);
		builder.setResourcePath(path);
		builder.setFolder(true);
		builder.setProjectName(context.getProjectName());
		builder.setApplicationScope(ApplicationScope.DESIGNER);
		builder.setVersion(0);
		ProjectResource pr = builder.build();
		DesignableProject dp = context.getProject();
		try {
			dp.createResource(pr);
		}
		catch(ResourceNamingException rne) {
			log.warnf("%s.createProjectResource: naming exception (%s)",CLSS,rne.getLocalizedMessage());
		}
		return builder.build();
	}
	// Create a diagram resource and add it to the project. The data is a JSON representation
	// of the resource.
	private ProjectResource createDiagramResource(String parent,String name,byte[] data) {
		ProjectResourceBuilder builder = ProjectResource.newBuilder();
		ResourceType rtype = BLTProperties.DIAGRAM_RESOURCE_TYPE;   // blt.diagram
		ProjectResourceId resourceId = new ProjectResourceId(context.getProjectName(),rtype,null);
		builder.setResourceId(resourceId);
		ResourcePath path = new ResourcePath(rtype,parent+"/"+name);
		builder.setApplicationScope(ApplicationScope.DESIGNER);
		builder.setResourcePath(path);
		builder.setFolder(false);
		builder.putData(data);
		builder.setProjectName(context.getProjectName());
		builder.setVersion(0);
		ProjectResource pr = builder.build();
		DesignableProject dp = context.getProject();
		try {
			dp.createResource(pr);
		}
		catch(ResourceNamingException rne) {
			log.warnf("%s.createProjectResource: naming exception (%s)",CLSS,rne.getLocalizedMessage());
		}
		return builder.build();
	}
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	
	/**
	 *  Create or modify the resource, depending on what is supplied.
	 */
	@Override
	public void run() {
		
			try {
				DesignableProject project = context.getProject();
	
					project.createOrModify(resource);
				
		
				GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
				ChangeOperation.ModifyResourceOperation co = ChangeOperation.ModifyResourceOperation.newModifyOp(resource,resource.getResourceSignature());
				List<ChangeOperation> ops = new ArrayList<>();
				ops.add(co);
				gw.pushProject(ops);
			}
			catch(ResourceNotFoundException rnf) {
				log.warnf("%s.run: Project resource not found %s:%s (%s)",CLSS,resource.getResourceId().getProjectName(),
						resource.getResourceId().getResourcePath().getPath().toString(),rnf.getMessage());
			}
			catch(Exception ex) {
				String msg = String.format("%s.run: Exception creating resource %s %s:%s (%s)", CLSS,resourceName,resource.getResourceId().getProjectName(),
						resource.getResourceId().getResourcePath().getPath().toString(),ex.getMessage());
				log.warn(msg,ex);
			}
		
	}
}
