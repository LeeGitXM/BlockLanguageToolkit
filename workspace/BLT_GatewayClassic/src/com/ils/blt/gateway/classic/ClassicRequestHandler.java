/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.classic;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.proxy.ProxyHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.annotation.ExecutableBlock;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.BasicDiagram;
import com.ils.blt.gateway.BasicRequestHandler;
import com.ils.blt.gateway.PythonRequestHandler;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.common.ClassList;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  This handler provides is a common class for handling requests for block properties and control
 *  of the execution engine. The requests can be expected arrive both through the scripting interface
 *  and the RPC diispatcher.In general, the calls are made to update properties 
 *  in the block objects and to trigger their evaluation.
 */
public class ClassicRequestHandler extends BasicRequestHandler implements ToolkitRequestHandler  {
	private final static String TAG = "ClassicRequestHandler";
	private final PythonRequestHandler pyHandler;
    
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	public ClassicRequestHandler(GatewayContext ctx) {
		super(ctx,BLTProperties.CLASSIC_MODULE_ID);
		pyHandler = new PythonRequestHandler(this);
	}


	/**
	 * Create an instance of a named class. If the class is not found in the JVM, try Python.
	 * Note: This is not part of the ToolkitRequestHandler interface. 
	 * @param key
	 * @param className
	 * @return the instance created, else null
	 */
	public ProcessBlock createInstance(String className,UUID parentId,UUID blockId) {
		
		log.infof("%s.createInstance of %s (%s:%s)",TAG,className,(parentId==null?"null":parentId.toString()),blockId.toString());
		ProcessBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,blockId);
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
	
	@Override
	public String getApplicationName(String uuid) {
		ProcessApplication app = pyHandler.getApplication(uuid);
		return app.getName();
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
				if( obj instanceof ProcessBlock ) {
					PalettePrototype bp = ((ProcessBlock)obj).getBlockPrototype();
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
		String state = "UNKNOWN";
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			BasicDiagram diagram = controller.getDiagram(diagramUUID);
			for(CoreBlock block:diagram.getDiagramBlocks()) {
				if( block.getName().equalsIgnoreCase(blockName) && block instanceof ProcessBlock) {
					state = ((ProcessBlock)block).getState().name();
					break;
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlockState: Diagram UUID string is illegal (%s)",TAG,diagramId);
		}
		return state;
	}
	
	@Override
	public String getFamilyName(String uuid) {
		ProcessFamily fam = pyHandler.getFamily(uuid);
		return fam.getName();
	}
	
	/**
	 * Set the state of every diagram in an application to the specified value.
	 * @param appname
	 * @param state
	 */
	@Override
	public void setApplicationState(String appname, String state) {
		try {
			DiagramState ds = DiagramState.valueOf(state.toUpperCase());
			for(SerializableResourceDescriptor srd:getDiagramDescriptors()) {
				ProcessApplication app = pyHandler.getApplication(srd.getId());
				if( app==null) continue;
				if( app.getName().equals(appname)) {
					UUID diagramuuid = UUID.fromString(srd.getId());
					BasicDiagram pd = controller.getDiagram(diagramuuid);
					if( pd!=null) {
						pd.setState(ds);    // Must notify designer
					}
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.setApplicationState: Illegal state (%s) supplied (%s)",TAG,state,iae.getMessage());
		}
	}

	// Handle all the intricasies of a property change
	protected void updateProperty(CoreBlock cb,BlockProperty existingProperty,BlockProperty newProperty) {
		if( !existingProperty.isEditable() )  return;

		if(cb instanceof ProcessBlock ){
			ProcessBlock block = (ProcessBlock)cb;
			log.debugf("%s.updateProperty old: %s, new:%s",TAG,existingProperty.toString(),newProperty.toString());
			if( !existingProperty.getBindingType().equals(newProperty.getBindingType()) ) {
				// If the binding has changed - fix subscriptions.
				controller.removeSubscription(block, existingProperty);
				existingProperty.setBindingType(newProperty.getBindingType());
				existingProperty.setBinding(newProperty.getBinding());
				controller.startSubscription(block,newProperty);
				// If the new binding is a tag write - do the write.
				if( !block.isLocked() && 
						(newProperty.getBindingType().equals(BindingType.TAG_READWRITE) ||
								newProperty.getBindingType().equals(BindingType.TAG_WRITE))	   ) {
					controller.updateTag(block.getParentId(),newProperty.getBinding(), new BasicQualifiedValue(newProperty.getValue()));
				}	
			}
			else if( !existingProperty.getBinding().equals(newProperty.getBinding()) ) {
				// Same type, new binding target.
				controller.removeSubscription(block, existingProperty);
				existingProperty.setBinding(newProperty.getBinding());
				controller.startSubscription(block,newProperty);
				// If the new binding is a tag write - do the write.
				if( !block.isLocked() && 
						(newProperty.getBindingType().equals(BindingType.TAG_READWRITE) ||
								newProperty.getBindingType().equals(BindingType.TAG_WRITE))	   ) {
					controller.updateTag(block.getParentId(),newProperty.getBinding(), new BasicQualifiedValue(newProperty.getValue()));
				}	
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

