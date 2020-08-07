package com.ils.blt.designer;

import java.util.Enumeration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.script.CommonScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
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
	protected final ApplicationRequestHandler requestHandler;
	private final GeneralPurposeTreeNode root;	      // Root of our save.
	private final CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
	
	public AuxiliaryDataSaveManager(GeneralPurposeTreeNode node) {
		this.root = node;
		this.requestHandler = new ApplicationRequestHandler();
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
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
			String database = requestHandler.getProductionDatabase();
			if( root.getState().equals(DiagramState.ISOLATED)) database = requestHandler.getIsolationDatabase();
			if(res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				SerializableApplication sa = null;
				try{
					byte[] bytes = res.getData();
					ObjectMapper mapper = new ObjectMapper();
					sa = mapper.readValue(new String(bytes), SerializableApplication.class);
					if( sa.getAuxiliaryData()!=null ) {
						// Save values back to the database
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
								sa.getId().toString(),sa.getAuxiliaryData(),database);
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
								sf.getId().toString(),sf.getAuxiliaryData(),database);
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
									sd.getId().toString(),sb.getAuxiliaryData(),database);
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
			String database = requestHandler.getProductionDatabase();
			if( root.getState().equals(DiagramState.ISOLATED)) database = requestHandler.getIsolationDatabase();
			extensionManager.runScript(context.getScriptManager(),className, ScriptConstants.PROPERTY_SET_SCRIPT, 
				block.getId().toString(),block.getAuxiliaryData(),database);
		}
		catch( Exception ex ) {
			log.errorf(TAG+".saveNode: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}

	}
}
