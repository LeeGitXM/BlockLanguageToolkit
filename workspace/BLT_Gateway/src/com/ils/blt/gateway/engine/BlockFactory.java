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

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  The block factory creates a concrete process block from a serializable version.
 */
public class BlockFactory  {
	private final static String TAG = "BlockFactory";
	private final LoggerEx log = LogUtil.getLogger(BlockFactory.class.getPackage().getName());
	private static BlockFactory instance = null;
	private final ProxyHandler proxyHandler;
	/**
	 * Private per the Singleton pattern.
	 */
	private BlockFactory() {
		proxyHandler = ProxyHandler.getInstance();
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
	 * @param projectId
	 * @param resourceId
	 * @param sb the block to be deserialized
	 * @return the ProcessBlock created from the specified SerializableBlock
	 */
	public ProcessBlock blockFromSerializable(UUID parentId,SerializableBlock sb) {
		String className = sb.getClassName();
		UUID blockId = sb.getId();
		log.debugf("%s.blockFromSerializable: Create instance of %s (%s)",TAG,className,blockId.toString());   // Should be updated
		ProcessBlock block = null;
		// If we can't create a Java class, try python ...
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,sb.getId());
		}
		catch(InvocationTargetException ite ) {
			log.warnf("%s.blockFromSerializable %s: Invocation failed (%s)",TAG,className,ite.getMessage()); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s.blockFromSerializable %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.infof("%s.blockFromSerializable: Class not found creating %s ... trying Python",TAG,className); 
			block = proxyHandler.createBlockInstance( className, parentId,blockId );
		}
		catch( InstantiationException ie ) {
			log.warnf("%s.blockFromSerializable: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s.blockFromSerializable: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
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
		pb.setName(sb.getName());
		BlockProperty[] properties = sb.getProperties();
		if( properties!=null ) {
			for( BlockProperty bp:properties) {
				BlockProperty property = pb.getProperty(bp.getName());
				if( property!=null ) {
					property.setEditable(bp.isEditable());
					property.setBinding(bp.getBinding());
					property.setBindingType(bp.getBindingType());
					property.setDisplayed(bp.isDisplayed());
					property.setDisplayOffsetX(bp.getDisplayOffsetX());
					property.setDisplayOffsetY(bp.getDisplayOffsetY());
					// Use the property change interface so as to properly trigger
					// local handling within the block (if the new value is non-null)
					property.setValue(bp.getValue());
					if( property.getValue()!=null ) {
						BlockPropertyChangeEvent event = 
								new BlockPropertyChangeEvent(pb.getBlockId().toString(),property.getName(),
										property.getValue(),bp.getValue());
							pb.propertyChange(event);
					}
				}
				else {
					log.warnf("%s: updateBlockFromSerializable: Property %s not found in process block %s",TAG,bp.getName(),pb.getName());
					log.warnf("     available names are: %s",pb.getPropertyNames().toString()); 
				}
			}
		}
		else {
			log.errorf("%s: updateBlockFromSerializable: No properties found in process block",TAG);
		}
	}
	
}
