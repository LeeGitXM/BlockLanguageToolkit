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
			// If the block is a sink, remap its tag path to correspond to its name.
			// If the tag path is currently empty, do nothing. This is an unconfigured block.
			if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) && 
					!isSystemName(block.getClassName(),block.getName() )) {
				// Make a list of sources before we edit the sink tags
				List<ProcessBlock> sources = handler.listSourceBlocksForSink(diagram.getSelf().toString(), block.getBlockId().toString());
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
					try {
						Thread.sleep(1000);
					}
					catch(InterruptedException ignore) {}
					path = String.format("[%s]%s/%s",isolationProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,block.getName());
					handler.createTag(DataType.String, path);
					if(diagram.getState().equals(DiagramState.ISOLATED)) prop.setBinding(path);
					controller.sendPropertyBindingNotification(block.getBlockId().toString(), prop.getName(), prop.getBinding());
					controller.startSubscription(diagram.getState(),block,prop);

					// Now edit sources - the source will have the same tag property as the sink
					for(ProcessBlock source:sources) {
						BlockProperty sourceProp = source.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
						if(sourceProp!=null) {
							if(sourceProp.getBinding()!=null && !sourceProp.getBinding().isEmpty() ) {
								controller.removeSubscription(source,sourceProp);
							}
							sourceProp.setBinding(prop.getBinding());
							sourceProp.setValue(prop.getValue());
							controller.sendPropertyBindingNotification(source.getBlockId().toString(), prop.getName(), prop.getBinding());
							controller.startSubscription(diagram.getState(),source,prop);
							String name = nameFromBinding(path);
							source.setName(name);
							controller.sendNameChangeNotification(source.getBlockId().toString(),name);
							break;
						}
					}
				}
			
				else {
					log.warnf("%s.synchBlocks: Sink %s does not have a tag path property",CLSS,block.getName());
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
	/**
	 * @param tpath
	 * @return a block name derived from a tag path
	 */
	private String nameFromBinding(String tpath) {
		String name = "";
		int pos = tpath.lastIndexOf("/");
		if( pos>0 ) {
			name = tpath.substring(pos+1);
		}
		else {
			pos = tpath.lastIndexOf("]");
			if( pos>0 ) {
				name = tpath.substring(pos+1);
			}
		}
		return name;
	}
}