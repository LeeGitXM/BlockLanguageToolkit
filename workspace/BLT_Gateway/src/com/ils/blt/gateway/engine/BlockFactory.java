/**
 *   (c) 2012-2015  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.gateway.engine;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.serializable.SerializableAnchor;
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
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private final ProxyHandler proxyHandler;
	/**
	 * Private per the Singleton pattern.
	 */
	private BlockFactory() {
		proxyHandler = ProxyHandler.getInstance();
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 * @return the static singleton instance
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
	 * @param parentId identifier of parent, the diagram
	 * @param sb the block to be deserialized
	 * @param projectId needed to find the correct script manager in the event of a block created from Python
	 * @return the ProcessBlock created from the specified SerializableBlock
	 */
	public ProcessBlock blockFromSerializable(UUID parentId,SerializableBlock sb,long projectId) {
		String className = sb.getClassName();
		UUID blockId = sb.getId();
		log.infof("%s.blockFromSerializable: Create instance of %s (%s)",TAG,className,blockId.toString());   // Should be updated
		ProcessBlock block = null;
		// If we can't create a Java class, try python ...
		try {
			Class<?> clss = Class.forName(className);
			log.infof("A");
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			log.infof("B");
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,sb.getId());
			log.infof("C");
		}
		catch(InvocationTargetException ite ) {
			log.warnf("%s.blockFromSerializable %s: Invocation failed (%s)",TAG,className,ite.getMessage()); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s.blockFromSerializable %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.debugf("%s.blockFromSerializable: No java class %s ... %s trying Python",TAG,className,sb.getName()); 
			block = proxyHandler.createBlockInstance(className,parentId,blockId,projectId,sb.getName());
		}
		catch( InstantiationException ie ) {
			log.warnf("%s.blockFromSerializable: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s.blockFromSerializable: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}

		if( block!=null ) {
			block.setTimer(controller.getTimer());  // Initial value
			updateBlockFromSerializable(block,sb);
		}
		return block;
	}
	
	/**
	 * Update the concrete instance of a Process block from a serializable block. Properties may be null for an
	 * uninitialized new block.
	 *
	 * @param pb process block, the result
	 * @param sb serializable block, the source
	 */
	public void updateBlockFromSerializable(ProcessBlock pb,SerializableBlock sb) {
		log.infof("In updateBlockFromSerializable, setting the name...");
		pb.setName(sb.getName());
		log.infof("...the name has been set...");
		// Update anchors first.  We do this because in some blocks property update behavior depends
		// on the datatype of the anchors.
		SerializableAnchor[] sanchors = sb.getAnchors();
		if( sanchors!=null ) {
			List<AnchorPrototype> descriptors = new ArrayList<>();
			for(SerializableAnchor sa:sanchors) {
				AnchorPrototype proto = new AnchorPrototype(sa.getDisplay(),sa.getDirection(),sa.getConnectionType());
				proto.setAnnotation(sa.getAnnotation());
				proto.setHint(sa.getHint());
				proto.setHidden(sa.isHidden());
				proto.setIsMultiple(sa.isMultiple());
				proto.setSortOrder(sa.getSortOrder());
				descriptors.add(proto);
			}
			pb.setAnchors(descriptors);
		}
		else {
			// A "Note" has no anchors. Others initialize anchor points themselves.
			log.infof("%s.updateBlockFromSerializable: No anchors found in process block",TAG);
		}
		
		log.infof("setting properties...");
		log.infof("     ...available properties are: %s",pb.getPropertyNames().toString()); 
		BlockProperty[] properties = sb.getProperties();
		if( properties!=null ) {
			for( BlockProperty bp:properties) {
				if( bp==null || bp.getName()==null) continue;
				log.infof("   Processing property: %s", bp.getName());
				BlockProperty property = pb.getProperty(bp.getName());
				if( property!=null ) {
					// Use the property change interface so as to properly trigger
					// local handling within the block (if the new value is non-null)
					// For bound variables, a binding change does not equate to a value change
					boolean valueChange = false;
					if( !property.getBindingType().equals(bp.getBindingType()) ) {
						if( BindingType.NONE.equals(bp.getBindingType()) ) {
							valueChange=true;
						}
					}
					else if( !property.getBindingType().equals(BindingType.NONE) &&
							 !property.getBindingType().equals(BindingType.OPTION) &&
							 !property.getBinding().equals(bp.getBinding()) ) {
						// Same type, new binding target - force the value to be different
						valueChange=true;
					}
					else if( BindingType.NONE.equals(property.getBindingType()) ||
							 BindingType.OPTION.equals(property.getBindingType())    ) {
						if( property.getValue()!=null && bp.getValue()!=null && !property.getValue().equals(bp.getValue())) {
							valueChange=true;
						}
					}
					property.setEditable(bp.isEditable());
					property.setBinding(bp.getBinding());
					property.setBindingType(bp.getBindingType());
					
					if( valueChange   ) {
						property.setValue(bp.getValue());
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
			log.errorf("%s.updateBlockFromSerializable: No properties found in process block",TAG);
		}
		log.infof("...done setting properties!");
		// Update Aux data
		pb.setAuxiliaryData(sb.getAuxiliaryData());
	}
}
