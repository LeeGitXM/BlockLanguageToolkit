package com.ils.blt.gateway;

import java.util.List;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.common.tag.TagUtility;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * Handle synchronization between a sink or source and its tag path. The user does not have the
 * ability to change the block's tag path. This is handled automatically here. For other types of
 * blocks this class does nothing. We reconcile the serializable blocks before creating the real ones.
 *   - block creation: create tag based on the block name
 *	 - block deletion: delete the associated tag
 *	 - rename: rename the tag to match the new block name
 */
public class BlockTagSynchronizer {
	private static final String CLSS = "BlockTagSynchronizer";
	private static final LoggerEx log = LogUtil.getLogger(BlockTagSynchronizer.class.getPackage().getName());
	private final ControllerRequestHandler handler;
	private final String productionProvider;
	private final String isolationProvider;

	
	public BlockTagSynchronizer() {
		this.handler = ControllerRequestHandler.getInstance();
		this.isolationProvider = handler.getIsolationTagProvider();
		this.productionProvider= handler.getProductionTagProvider();
	}
	
	/**
	 * The blocks in the diagram may have been newly created or modified. For any sources or sinks, 
	 * create tags corresponding to the block name. Any tags that already exist should fail creation silently.
	 * 
	 * @param diagram
	 */
	public void synchBlocks(ProcessDiagram diagram) {
		BlockExecutionController controller = BlockExecutionController.getInstance();
		for(ProcessBlock block:diagram.getBlocks()) {
			// If the block is a source, remap its tag to what the corresponding sink will be.
			// Do not create the tag. This is done by the sink
			if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) {
				BlockProperty prop = null;
				for( BlockProperty property:block.getProperties() ) {
					if(property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TAG_PATH)) {
						prop = property;
						break;
					}
				}
				if(prop != null ) {
					List<SerializableBlockStateDescriptor> sinks = handler.listSinksForSource(diagram.getSelf().toString(), block.getBlockId().toString());
					if(sinks.size()>0) {
						SerializableBlockStateDescriptor sink = sinks.get(0);  // There isn't supposed to be more than one sink for a source
						String path = String.format("[%s]%s/%s",productionProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,sink.getName());
						controller.removeSubscription(block,prop);
						if(!diagram.getState().equals(DiagramState.ISOLATED)) prop.setBinding(path);
						path = String.format("[%s]%s/%s",isolationProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,block.getName());
						if(diagram.getState().equals(DiagramState.ISOLATED))  prop.setBinding(path);
						controller.startSubscription(diagram.getState(),block,prop);
					}
				}
				else {
					log.warnf("%s.synchBlocks: Source %s does not have a tag path property",CLSS,block.getName());
				}
			}

			// If the block is a sink, remap its tag path to correspond to its name.
			// If the tag path is currently empty, do nothing. This is an unconfigured block.
			else if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) && 
					!isSystemName(block.getClassName(),block.getName() )) {
				BlockProperty prop = null;
				for( BlockProperty property:block.getProperties() ) {
					if(property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TAG_PATH)) {
						prop = property;
						break;
					}
				}
				if(prop != null ) { 
					if(prop.getBinding()!=null && !prop.getBinding().isEmpty() ) {
						controller.removeSubscription(block,prop);
					}
					String path = String.format("[%s]%s/%s",productionProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,block.getName());
					handler.createTag(DataType.String, path);
					if(!diagram.getState().equals(DiagramState.ISOLATED)) prop.setBinding(path);
					path = String.format("[%s]%s/%s",isolationProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,block.getName());
					handler.createTag(DataType.String, path);
					if(diagram.getState().equals(DiagramState.ISOLATED)) prop.setBinding(path);
					prop.setBinding(path);
					controller.sendPropertyBindingNotification(block.getBlockId().toString(), prop.getName(), prop.getBinding());
					controller.startSubscription(diagram.getState(),block,prop);
				}
				else {
					log.warnf("%s.synchBlocks: Source %s does not have a tag path property",CLSS,block.getName());
				}
			}
		}
	}
	/**
	 * For a sink, but not a source, delete the associated tag. We have to delete
	 * both production and isolation tags.
	 * @param block
	 */
	public void synchDeletedBlock(ProcessBlock block) {
		if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)) {
			BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if(prop == null ) return;
			String path = prop.getBinding();
			handler.deleteTag(TagUtility.replaceProviderInPath(isolationProvider,path));
			handler.deleteTag(TagUtility.replaceProviderInPath(productionProvider,path));
		}
	}
	
	/**
	 * Return true if the name seems like a system generated name, that is: <block name>-nnn.
	 * @param name
	 * @return true if the block name is probably system-generated
	 */
	private boolean isSystemName(String className,String name) {
		boolean isSystem = false;
		int pos = className.lastIndexOf(".");
		if( pos>=0 )  className = className.substring(pos+1);
		className = className.toUpperCase();
		if(name.startsWith(className)) {
			pos=name.indexOf("-");
			if( pos>=0) {
				String suffix = name.substring(pos+1);
				try {
					int index = Integer.parseInt(suffix);
					if(index>0) isSystem = true;
				}
				catch(NumberFormatException nfe) {}
			}
		}
		return isSystem;
		
	}
}