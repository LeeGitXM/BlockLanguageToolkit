package com.ils.blt.common.block;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.control.NotificationChangeListener;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 * Hold an attribute of a block. This class is designed to be serializable via
 * the Jackson JSON serializer. This is the reason that the get/set methods 
 * use the concrete SerializedQualifiedValue class rather than the interface.
 */
public class BlockProperty implements NotificationChangeListener {
	private static final String TAG = "BlockProperty";
	private static LoggerEx log = LogUtil.getLogger(PalettePrototype.class.getPackage().getName());
	private boolean editable;
	private PropertyType type = PropertyType.STRING;
	private String binding = "";
	private BindingType bindingType = BindingType.NONE;
	private boolean displayed   = false;
	private int displayOffsetX = 30;
	private int displayOffsetY = 50;

	private String name;

	private Object value = null;
	private List<ChangeListener> changeListeners = new ArrayList<ChangeListener>();

	/** 
	 * Constructor: Sets all attributes.
	 */
	public BlockProperty(String name,Object val,PropertyType type,boolean canEdit) {
		if(val==null) throw new IllegalArgumentException("null property not allowed");
		if(val instanceof QualifiedValue) throw new IllegalArgumentException(String.format("Complex object %s not allowed",val.getClass().getName()));
		this.name = name;
		this.value = val;
		this.type = type;
		this.editable = canEdit;
	}
	
	public BlockProperty() {
	}
	
	public void addChangeListener(ChangeListener c) {
		changeListeners.add(c);
	}
	
	public void removeChangeListener(ChangeListener c) {
		changeListeners.remove(c);
	}
	
	private void notifyChangeListeners() {
		ChangeEvent event = new ChangeEvent(this);
		for(ChangeListener l: changeListeners) {
			l.stateChanged(event);
		}
	}
	/**
	 * Deserialize from a Json string 
	 * @param json
	 * @return the BlockProperty created from the input JSON string
	 */
	public static BlockProperty createProperty(String json) {
		BlockProperty property = new BlockProperty();
		if( json!=null && json.length()>0 )  {
			ObjectMapper mapper = new ObjectMapper();

			try {
				property = mapper.readValue(json, BlockProperty.class);
			} 
			catch (JsonParseException jpe) {
				log.warnf("%s: createProperty parse exception from %s (%s)",TAG,json,jpe.getLocalizedMessage());
			}
			catch(JsonMappingException jme) {
				log.warn(String.format("%s: createProperty from %s mapping exception (%s)",TAG,json,jme.getLocalizedMessage()),jme);
			}
			catch(IOException ioe) {
				log.warnf("%s: createProperty from %s IO exception (%s)",TAG,json,ioe.getLocalizedMessage());
			} 
		}
		return property;
	}
	
	public boolean isEditable() {return editable;}
	public void setEditable(boolean editable) {this.editable = editable;}
	public String getBinding() {return binding;}
	public void setBinding(String lnk) {this.binding = lnk;}
	public String getName() {return name;}
	public void setName(String name) {this.name = name;}
	public boolean isDisplayed() {return displayed;}
	public void setDisplayed(boolean shown) {this.displayed = shown;}
	public int getDisplayOffsetX() {return displayOffsetX;}
	public void setDisplayOffsetX(int externalOffsetX) {this.displayOffsetX = externalOffsetX;}
	public int getDisplayOffsetY() {return displayOffsetY;}
	public void setDisplayOffsetY(int externalOffsetY) {this.displayOffsetY = externalOffsetY;}
	public BindingType getBindingType() { return bindingType; }
	public void setBindingType(BindingType type) { this.bindingType = type; }
	public PropertyType getType() {return type;}
	public void setType(PropertyType type) {this.type = type;}
	public Object getValue() {return value;}
	public void setValue(Object obj) {
		if(obj==null) throw new IllegalArgumentException("null property not allowed");
		if(obj instanceof QualifiedValue) throw new IllegalArgumentException(String.format("Complex object %s not allowed",obj.getClass().getName()));
		this.value = obj;
		notifyChangeListeners();
	}
	
	/**
	 * Serialize into a JSON string
	 */
	public String toJson() {
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(this);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",TAG,ge.getMessage());
		}
		log.tracef("%s: toJson = %s",TAG,json);
		return json;
	}
	
	/**
	 * A readable string representation for debugging.
	 */
	public String toString() {
		return String.format("%s=%s (%s)",getName(),value==null?"null":value.toString(),bindingType.name());
	}

	/**
	 * Update a value based on a push notification. Note that this
	 * triggers any change listeners on this property. These
	 * notifications are currently NOT on the UI thread.
	 */
	@Override
	public void valueChange(QualifiedValue val) {
		if( val!=null && val.getValue()!=null) {
			setValue(val.getValue());
		}
	}
}
