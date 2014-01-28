/**
 *   (c) 2012  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.gateway.engine;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.BlockPropertyChangeEvent;
import com.ils.block.control.ExecutionController;
import com.ils.blt.common.serializable.SerializableBlock;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  The block factory creates a concrete process block from a serializable version.
 */
public class BlockFactory  {
	private final static String TAG = "BlockFactory";
	private final LoggerEx log = LogUtil.getLogger(BlockFactory.class.getPackage().getName());
	private static BlockFactory instance = null;
	/**
	 * Private per the Singleton pattern.
	 */
	private BlockFactory() {
		
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static BlockFactory getInstance() {
		if( instance==null) {
			synchronized(BlockFactory.class) {
				instance = new BlockFactory();
			}
		}
		return instance;
	}
	
	/**
	 * Create a concrete instance of a Process block represented by the serializable block.
	 * @param sb
	 * @return
	 */
	public ProcessBlock blockFromSerializable(long projectId,long resourceId,SerializableBlock sb) {
		String className = sb.getClassName();
		UUID blockId = sb.getId();
		log.debugf("%s: createInstance of %s (%d,%d,%s)",TAG,className,projectId,resourceId,blockId.toString());   // Should be updated
		ProcessBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,long.class,long.class,UUID.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),projectId,resourceId,blockId);
		}
		catch(InvocationTargetException ite ) {
			log.warnf("%s: blockFromSerializable %s: Invocation failed (%s)",TAG,className,ite.getMessage()); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s: blockFromSerializable %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.warnf("%s: blockFromSerializable: Error creating %s (%s)",TAG,className,cnf.getMessage()); 
		}
		catch( InstantiationException ie ) {
			log.warnf("%s: blockFromSerializable: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s: blockFromSerializable: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}
		if( block!=null ) updateBlockFromSerializable(block,sb);
		return block;
	}
	
	/**
	 * Update the concrete instance of a Process block from a serializable block. Properties may be null for an
	 * uninitialized new block.
	 *
	 * @param pb
	 * @param sb
	 */
	public void updateBlockFromSerializable(ProcessBlock pb,SerializableBlock sb) {
		pb.setLabel(sb.getLabel());
		BlockProperty[] properties = sb.getProperties();
		if( properties!=null ) {
			for( BlockProperty bp:properties) {
				BlockProperty property = pb.getProperty(bp.getName());
				if( property!=null && property.getValue()!=null ) {
					if( property.getQuality()==null) property.setQuality(Quality.Level.Good.toString());
					property.setEditible(bp.isEditible());
					property.setMaximum(bp.getMaximum());
					property.setMinimum(bp.getMinimum());
					property.setBinding(bp.getBinding());
					property.setBindingType(bp.getBindingType());
					// Use the property change interface so as to properly trigger
					// local handling within the block
					BlockPropertyChangeEvent event = 
						new BlockPropertyChangeEvent(pb.getBlockId().toString(),property.getName(),
							new BasicQualifiedValue(property.getValue(),
								new BasicQuality(property.getQuality(),Quality.Level.Good)),
							new BasicQualifiedValue(bp.getValue(),
								new BasicQuality(bp.getQuality(),Quality.Level.Good)));
					pb.propertyChange(event);

				}
				else {
					log.warnf("%s: updateBlockFromSerializable: Property %s not found in concrete instance",TAG,bp.getName()); 
				}
			}
		}
	}
	
}
