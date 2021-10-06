package com.ils.blt.gateway;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
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
	 * create tags corresponding to the block name. Any tags that already exists should fail creation silently.
	 * 
	 * @param diagram
	 */
	public void synchBlocks(SerializableDiagram diagram) {
		for(SerializableBlock block:diagram.getBlocks()) {
			if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ||
				block.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) {
				BlockProperty prop = null;
				for( BlockProperty property:block.getProperties() ) {
					if(property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TAG_PATH)) {
						prop = property;
						break;
					}
				}
				if(prop == null ) continue;
				String path = String.format("[%s]%s/%s",productionProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,block.getName());
				handler.createTag(DataType.String, path);
				if(!diagram.getState().equals(DiagramState.ISOLATED)) prop.setBinding(path);
				path = String.format("[%s]%s/%s",isolationProvider,BlockConstants.SOURCE_SINK_TAG_FOLDER,block.getName());
				handler.createTag(DataType.String, path);
				if(diagram.getState().equals(DiagramState.ISOLATED)) prop.setBinding(path);
			}
		}
	}
	/**
	 * For a source or sink, delete the associated tag. We have to delete
	 * both production and isolation tags.
	 * @param block
	 */
	public void synchDeletedBlock(ProcessBlock block) {
		if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ||
			block.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) {
			BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if(prop == null ) return;
			String path = prop.getBinding();
			handler.deleteTag(TagUtility.replaceProviderInPath(isolationProvider,path));
			handler.deleteTag(TagUtility.replaceProviderInPath(productionProvider,path));
		}
	}
}