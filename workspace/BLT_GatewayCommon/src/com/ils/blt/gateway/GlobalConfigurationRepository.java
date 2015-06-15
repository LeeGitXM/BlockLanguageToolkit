/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.BasicDiagram;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.tag.TagListener;
import com.ils.blt.gateway.tag.TagReader;
import com.ils.blt.gateway.tag.TagWriter;
import com.ils.common.BoundedBuffer;
import com.ils.common.watchdog.AcceleratedWatchdogTimer;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.project.ProjectListener;

/**
 * This class is a cache of configuration settings. The repository is
 * a Singleton for ease of access throughout the application. These
 * are designer-scope settings.
 *
 */
public class GlobalConfigurationRepository  {
	private final static String TAG = "GlobalConfigurationRepository";
	private static GlobalConfigurationRepository instance = null;
	private ToolkitRequestHandler requestHandler;
	private Map<String,ConfigurationElement> repo;
	private final LoggerEx log;
	
	public final static int DATATYPE_COLOR = 0;
	public final static int DATATYPE_STRING = 1;



	/**
	 * Private per Singleton pattern
	 */
	private GlobalConfigurationRepository() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		repo = new HashMap<>();
	}
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static GlobalConfigurationRepository getInstance() {
		if( instance==null) {
			synchronized(GlobalConfigurationRepository.class) {
				instance = new GlobalConfigurationRepository();
			}
		}
		return instance;
	}
	/**
	 * The request handler must be specified before the controller is useful.
	 */
	public void setRequestHandler(ToolkitRequestHandler h) {
		this.requestHandler = h;
	}

	public Object getConfigurationElement(String key) {
		Object result = null;
		ConfigurationElement element = repo.get(key);
		if( element!=null) result = element.getValue();
		return result;
	}
	
	// The settings are mapped each time we start up.
	public void setConfigurationElement(String k,String key,int datatype,Object def) {
		// Query the internal database for a value. If none, then use the supplied default.
		String val = requestHandler.getToolkitProperty(key);  // Use the specific key,
		if( datatype==DATATYPE_STRING ) {
			String value = requestHandler.getToolkitProperty(key);
			if( value ==null ) value = def.toString();
			ConfigurationElement element = new ConfigurationElement(key,datatype,value);
			repo.put(key, element);
		}
		else {
			log.errorf("%s.setConfigurationElement: %s unrecognized datatype (%d)",TAG,key,datatype);
		}
	}

	// ====================================== ProjectResourceKey =================================
	/**
	 * Class for keyed storage of a configuration item.
	 */
	private class ConfigurationElement {
		private final String key;     // The specific key
		private final int datatype;
		private final Object value;
		public ConfigurationElement(String k,int dt,Object val) {
			this.key = k;
			this.datatype = dt;
			this.value = val;
		}
		public String getKey() { return key; }
		public int getDataType() { return datatype; }
		public Object getValue() { return value; }

		// So that class may be used as a map key
		// Same projectId and resourceId is sufficient to prove equality
		@Override
		public boolean equals(Object arg) {
			boolean result = false;
			if( arg instanceof ConfigurationElement) {
				ConfigurationElement that = (ConfigurationElement)arg;
				if( (this.key==that.getKey())    ) {
					result = true;
				}
			}
			return result;
		}
		@Override
		public int hashCode() {
			return this.key.hashCode()+datatype;
		}
	}
}
