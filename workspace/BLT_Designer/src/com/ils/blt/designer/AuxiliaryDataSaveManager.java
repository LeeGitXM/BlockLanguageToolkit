package com.ils.blt.designer;

import java.util.Enumeration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 * Search the descendants of the specified node, looking for nodes that contain auxiliary data.
 * Save the contents of those structures into the external universe (database).
 * 
 * @author chuckc
 *
 */
public class AuxiliaryDataSaveManager implements Runnable {
	private static final String TAG = "AuxiliaryDataSaveManager";
	private static final LoggerEx log = LogUtil.getLogger(AuxiliaryDataSaveManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final ClientScriptExtensionManager extensionManager = ClientScriptExtensionManager.getInstance();
	
	public AuxiliaryDataSaveManager(AbstractResourceNavTreeNode node) {
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
		recursivelySave(root);
	}
	
	// Recursively descend the node tree, looking for resources that have 
	// auxiliary data. When found, save that data into the database.
	private void recursivelySave(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		if( res!=null ) {
			if(res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				SerializableApplication sa = null;
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sa = mapper.readValue(new String(bytes), SerializableApplication.class);
					if( sa.getAuxiliaryData()!=null ) {
						// Save values back to the database
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
								sa.getId().toString(),sa.getAuxiliaryData());
					}
				}
				catch(Exception ex) {
					log.warnf("%s.recursivelySave: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
			else if(res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				SerializableFamily sf = null;
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sf = mapper.readValue(new String(bytes), SerializableFamily.class);
					if( sf.getAuxiliaryData()!=null ) {
						// Save values back to the database
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
								sf.getId().toString(),sf.getAuxiliaryData());
					}
				}
				catch(Exception ex) {
					log.warnf("%s.recursivelySave: Deserialization exception (%s)",TAG,ex.getMessage());
				}
			}
			else if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				// Iterate over blocks
				SerializableDiagram sd = null;
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
					// Loop over blocks in the diagram looking for ones with AUX data.
					// If it has AUX data, attempt to save ...
					for( SerializableBlock sb: sd.getBlocks()) {
						if( sb.getAuxiliaryData()!=null ) {
							extensionManager.runScript(context.getScriptManager(), sb.getClassName(), ScriptConstants.PROPERTY_SET_SCRIPT, 
									sd.getId().toString(),sb.getAuxiliaryData());
						}
					}
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
			recursivelySave((AbstractResourceNavTreeNode)child);
		}
	}
	
	/**
	 * Copy auxiliary data from the database into the node. The entire root
	 * node hierarchy will be saved as project resources by the calling entity.
	 */
	private void saveNode(ProcessBlockView block, String className) {
		try {
			extensionManager.runScript(context.getScriptManager(),className, ScriptConstants.PROPERTY_SET_SCRIPT, 
				block.getId().toString(),block.getAuxiliaryData());
		}
		catch( Exception ex ) {
			log.errorf(TAG+".saveNode: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}

	}
}
