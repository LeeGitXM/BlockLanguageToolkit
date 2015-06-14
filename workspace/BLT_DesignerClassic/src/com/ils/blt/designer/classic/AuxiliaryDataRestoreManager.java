package com.ils.blt.designer.classic;

import java.util.Enumeration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 * Search the descendants of the specified node, looking for nodes that contain auxiliary data.
 * Restore the contents of those structures from the external universe (database).
 * 
 * @author chuckc
 *
 */
public class AuxiliaryDataRestoreManager implements Runnable {
	private static final String TAG = "AuxiliaryDataRestoreManager";
	private static final LoggerEx log = LogUtil.getLogger(AuxiliaryDataRestoreManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	
	public AuxiliaryDataRestoreManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.root = node;
		this.workspace = wksp;
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	
	@Override
	public void run() {
		recursivelyRestore(root);
	}
	
	// Recursively descend the node tree, looking for resources that have 
	// auxiliary data. When found, restore that data into the node.
	private void recursivelyRestore(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		
		if( res!=null ) {
			if(res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				SerializableApplication sa = null;
				ResourceUpdateManager upmgr = new ResourceUpdateManager(workspace,res);
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sa = mapper.readValue(new String(bytes), SerializableApplication.class);
					GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
					// Update from database, then save the resource
					extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
							sa.getId().toString(),auxData);
					if( !auxData.containsData()) {
						sa.setAuxiliaryData(auxData);
						upmgr.run();
					}

				}
				catch(Exception ex) {
					log.warnf("%s.recursivelySave: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
			else if(res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				SerializableFamily sf = null;
				ResourceUpdateManager upmgr = new ResourceUpdateManager(workspace,res);
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sf = mapper.readValue(new String(bytes), SerializableFamily.class);
					GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
					// Save values back to the database
					extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
							sf.getId().toString(),auxData);
					if( !auxData.containsData()) {
						sf.setAuxiliaryData(auxData);
						upmgr.run();
					}
				}
				catch(Exception ex) {
					log.warnf("%s.recursivelySave: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
			else if(res.getResourceType().equals(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE) ) {
				// Iterate over blocks
				SerializableDiagram sd = null;
				ResourceUpdateManager upmgr = new ResourceUpdateManager(workspace,res);
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
					boolean hasUpdate = false;
					// Loop over blocks in the diagram looking for ones with AUX data.
					// If it has AUX data, attempt to save ...
					for( SerializableBlock sb: sd.getBlocks()) {
						GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
						extensionManager.runScript(context.getScriptManager(), sb.getClassName(), ScriptConstants.PROPERTY_GET_SCRIPT, 
								sd.getId().toString(),auxData);
						if( !auxData.containsData()) {
							sb.setAuxiliaryData(auxData);
							hasUpdate = true;
						}
					}
					if( hasUpdate ) upmgr.run();
				}
				catch(Exception ex) {
					log.warnf("%s.recursivelySave: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
		}
		
		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			recursivelyRestore((AbstractResourceNavTreeNode)child);
		}
	}
}
