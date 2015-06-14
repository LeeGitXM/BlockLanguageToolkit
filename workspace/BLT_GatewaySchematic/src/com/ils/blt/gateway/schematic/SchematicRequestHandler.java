/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.schematic;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.gateway.common.BasicDiagram;
import com.ils.blt.gateway.common.BasicRequestHandler;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.common.ClassList;
import com.ils.common.annotation.ExecutableBlock;
import com.ils.sblock.SchematicBlock;
import com.ils.sblock.proxy.ProxyHandler;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  This handler provides is a common class for handling requests for block properties and control
 *  of the execution engine. The requests can be expected arrive both through the scripting interface
 *  and the RPC diispatcher.In general, the calls are made to update properties 
 *  in the block objects and to trigger their evaluation.
 */
public class SchematicRequestHandler extends BasicRequestHandler implements ToolkitRequestHandler  {
	private final static String TAG = "SchematicRequestHandler";
	private final PythonRequestHandler pyHandler;
    
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	public SchematicRequestHandler(GatewayContext ctx) {
		super(ctx);
		pyHandler = new PythonRequestHandler(this);
	}


	/**
	 * Create an instance of a named class. If the class is not found in the JVM, try Python.
	 * Note: This is not part of the ToolkitRequestHandler interface. 
	 * @param key
	 * @param className
	 * @return the instance created, else null
	 */
	public SchematicBlock createInstance(String className,UUID parentId,UUID blockId) {
		
		log.infof("%s.createInstance of %s (%s:%s)",TAG,className,(parentId==null?"null":parentId.toString()),blockId.toString());
		SchematicBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			block = (SchematicBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,blockId);
		}
		catch(InvocationTargetException ite ) {
			Throwable cause = ite.getCause();
			log.warn(String.format("%s.createInstance %s: Invocation of constructor failed (%s)",TAG,
					className,(cause==null?"No cause available":cause.getLocalizedMessage())),cause); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s.createInstance %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.infof("%s.createInstance: Java class %s not found - trying Python",TAG,className);
			ProxyHandler ph = ProxyHandler.getInstance();
			BasicDiagram diagram = controller.getDiagram(parentId);
			if( diagram!=null ) {
				long projectId = diagram.getProjectId();
				block = ph.createBlockInstance(className,parentId,blockId,projectId);
			}
		}
		catch( InstantiationException ie ) {
			log.warnf("%s.createInstance: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s.createInstance: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}
		return block;
	}

	
	/**
	 * Query the block controller for a block specified by the block id. If the block
	 * does not exist, create it.
	 * 
	 * @param className
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 * @return the properties of an existing or new block.
	 */
	@Override
	public List<BlockProperty> getBlockProperties(String className,long projectId,long resourceId, UUID blockId) {
		// If the instance doesn't exist, create one
		log.debugf("%s.getBlockProperties of %s (%s)",TAG,className,blockId.toString());
		List<BlockProperty> results = new ArrayList<>();
		BasicDiagram diagram = controller.getDiagram(projectId, resourceId);
		CoreBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		BlockProperty[] props = null;
		if(block!=null) {
			props = block.getProperties();  // Existing block
			log.tracef("%s.getProperties existing %s = %s",TAG,block.getClass().getName(),props.toString());
		}
		else {
			block = createInstance(className,(diagram!=null?diagram.getSelf():null),blockId);  // Block is not (yet) attached to a diagram
			if(block!=null) {
				props = block.getProperties();
				log.tracef("%s.getProperties new %s = %s",TAG,block.getClass().getName(),props.toString());
			}
		}
		for(BlockProperty prop:props) {
			results.add(prop);
		}
		return results;
	}

	
	@Override
	public List<PalettePrototype> getBlockPrototypes() {
		List<PalettePrototype> results = new ArrayList<>();
		ClassList cl = new ClassList();
		List<Class<?>> classes = cl.getAnnotatedClasses(BLTProperties.BLOCK_JAR_NAME, ExecutableBlock.class,"com/ils/block/");
		for( Class<?> cls:classes) {
			log.debugf("   found block class: %s",cls.getName());
			try {
				Object obj = cls.newInstance();
				if( obj instanceof SchematicBlock ) {
					PalettePrototype bp = ((SchematicBlock)obj).getBlockPrototype();
					results.add(bp);
				}
				else {
					log.warnf("%s: Class %s not a ProcessBlock",TAG,cls.getName());
				}
			} 
			catch (InstantiationException ie) {
				log.warnf("%s.getBlockPrototypes: Exception instantiating block (%s)",TAG,ie.getLocalizedMessage());
			} 
			catch (IllegalAccessException iae) {
				log.warnf("%s.getBlockPrototypes: Access exception (%s)",TAG,iae.getMessage());
			}
			catch (Exception ex) {
				log.warnf("%s.getBlockPrototypes: Runtime exception (%s)",TAG,ex.getMessage(),ex);
			}
		}
		// Now add prototypes from Python-defined blocks
		// NOTE: We use the gateway script manager because these blocks do
		//       not yet exist in a diagram (or project).
		ProxyHandler phandler = ProxyHandler.getInstance();
		try {
			ScriptManager mgr = context.getScriptManager();
			List<PalettePrototype> prototypes = phandler.getPalettePrototypes(mgr);
			for( PalettePrototype pp:prototypes) {
				results.add(pp);
			}
		}
		catch (Exception ex) {
			log.warnf("%s.getBlockPrototypes: Runtime exception (%s)",TAG,ex.getMessage(),ex);
		}
		log.infof("%s.getBlockPrototypes: returning %d palette prototypes",TAG,results.size());
		return results;
	}
	
	@Override
	public String getBlockState(String diagramId, String blockName) {
		String state = "UNSET";
		return state;
	}


	// Handle all the intricasies of a property change. We do NOT initiate tag subscriptions.
	protected void updateProperty(CoreBlock cb,BlockProperty existingProperty,BlockProperty newProperty) {
		if( !existingProperty.isEditable() )  return;

		if(cb instanceof SchematicBlock ){
			SchematicBlock block = (SchematicBlock)cb;
			log.debugf("%s.updateProperty old: %s, new:%s",TAG,existingProperty.toString(),newProperty.toString());
			if( !existingProperty.getBindingType().equals(newProperty.getBindingType()) ) {
				existingProperty.setBindingType(newProperty.getBindingType());
				existingProperty.setBinding(newProperty.getBinding());
			}
			else if( !existingProperty.getBinding().equals(newProperty.getBinding()) ) {
				// Same type, new binding target.
				existingProperty.setBinding(newProperty.getBinding());
			}
			else {
				// The event came explicitly from the designer/client. Send event whether it changed or not.
				if( existingProperty.getBindingType().equals(BindingType.NONE) && newProperty.getValue()!=null   )   {
					log.debugf("%s.setProperty sending event ...",TAG);
					BlockPropertyChangeEvent event = new BlockPropertyChangeEvent(block.getBlockId().toString(),newProperty.getName(),
							existingProperty.getValue(),newProperty.getValue());
					block.propertyChange(event);
				}
			}
		}
	}
}

