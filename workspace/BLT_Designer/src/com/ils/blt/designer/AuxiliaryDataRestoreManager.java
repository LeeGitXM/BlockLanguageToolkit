package com.ils.blt.designer;

import java.util.Enumeration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
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
	protected final ApplicationRequestHandler requestHandler;
	private final GeneralPurposeTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	private final ClientScriptExtensionManager extensionManager = ClientScriptExtensionManager.getInstance();
	
	public AuxiliaryDataRestoreManager(DiagramWorkspace wksp,GeneralPurposeTreeNode node) {
		this.root = node;
		this.requestHandler = new ApplicationRequestHandler();
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
			String database = requestHandler.getProductionDatabase();
			if( root.getState().equals(DiagramState.ISOLATED)) database = requestHandler.getIsolationDatabase();
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
							sa.getId().toString(),auxData,database);
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
							sf.getId().toString(),auxData,database);
					if( !auxData.containsData()) {
						sf.setAuxiliaryData(auxData);
						upmgr.run();
					}
				}
				catch(Exception ex) {
					log.warnf("%s.recursivelySave: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
			else if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
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
								sd.getId().toString(),auxData,database);
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
	/**
	 * Copy auxiliary data from the database into the node. The entire root
	 * node hierarchy will be saved as project resources by the calling entity.
	 */
	private void restoreApplication(SerializableApplication app) {
		try {
			if(app.getAuxiliaryData()==null) app.setAuxiliaryData(new GeneralPurposeDataContainer());
			String database = requestHandler.getProductionDatabase();
			if( root.getState().equals(DiagramState.ISOLATED)) database = requestHandler.getIsolationDatabase();
			extensionManager.runScript(context.getScriptManager(),ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				app.getId().toString(),app.getAuxiliaryData(),database);
		}
		catch( Exception ex ) {
			log.errorf(TAG+".restoreNode: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}

	}
	
	/**
	 * Copy auxiliary data from the database into the node. The entire root
	 * node hierarchy will be saved as project resources by the calling entity.
	 */
	private void restoreNode(ProcessBlockView block, String className) {
		try {
			String database = requestHandler.getProductionDatabase();
			if( root.getState().equals(DiagramState.ISOLATED)) database = requestHandler.getIsolationDatabase();
			extensionManager.runScript(context.getScriptManager(),className, ScriptConstants.PROPERTY_GET_SCRIPT, 
				block.getId().toString(),block.getAuxiliaryData(),database);
		}
		catch( Exception ex ) {
			log.errorf(TAG+".restoreNode: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}

	}
}
