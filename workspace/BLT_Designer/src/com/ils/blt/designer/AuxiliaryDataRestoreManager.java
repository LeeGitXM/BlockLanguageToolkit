package com.ils.blt.designer;

import java.util.Enumeration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
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
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	
	public AuxiliaryDataRestoreManager(AbstractResourceNavTreeNode node) {
		this.root = node;
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
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					SerializableApplication sa = mapper.readValue(new String(bytes), SerializableApplication.class);
					restoreApplication(sa);
				}
				catch(Exception ex) {
					log.warnf("%s.deserializeApplication: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
			else if(res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				
			}
			else if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				// Iterate over blocks
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
			extensionManager.runScript(context.getScriptManager(),ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				app.getId().toString(),app.getAuxiliaryData());
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
			extensionManager.runScript(context.getScriptManager(),className, ScriptConstants.PROPERTY_GET_SCRIPT, 
				block.getId().toString(),block.getAuxiliaryData());
		}
		catch( Exception ex ) {
			log.errorf(TAG+".restoreNode: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}

	}
}
