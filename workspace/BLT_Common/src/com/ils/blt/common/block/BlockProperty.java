package com.ils.blt.common.block;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.notification.NotificationChangeListener;
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
	private int displayOffsetX = BlockConstants.DEFAULT_ATTRIBUTE_OFFSET_X;
	private int displayOffsetY = BlockConstants.DEFAULT_ATTRIBUTE_OFFSET_Y;

	private String name = "UNSET-NAME";
	private Object value = "UNSET-VALUE";
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
			//log.infof("%s.notifying ... %s of %s",TAG,l.getClass().getName(),value.toString());
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
				if( property.value==null) property.setValue("");    // May be wrong for property type
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
	 * Helper methods for dealing with list data types. The list is stored as a single
	 * delimited string - the delimiter is the first character. Clearly the delimiter 
	 * must be a character not present in the strings.
	 */
	public static String assembleList(List<String> list,String delimiter) {
		StringBuffer s = new StringBuffer();
		if( list!=null ) {
			for(String text:list) {
				s.append(delimiter);
				s.append(text);
			}
		}
		return s.toString();
	}
	public static List<String> disassembleList(String raw) {
		List<String> list = new ArrayList<>(); 
		if( raw!=null && raw.length()>0 ) {
			String delimiter = raw.substring(0, 1);
			raw = raw.substring(1);
			String[] subs = raw.split(delimiter);
			list = new ArrayList<String>(Arrays.asList(subs));
		}
		return list;
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
	 * @return a copy of this property
	 */
	@Override
	public BlockProperty clone() {
		BlockProperty clone = new BlockProperty(getName(),getValue(),getType(),isEditable());
		clone.setBinding(getBinding());
		clone.setDisplayed(isDisplayed());
		clone.setDisplayOffsetX(getDisplayOffsetX());
		clone.setDisplayOffsetY(getDisplayOffsetY());
		clone.setBindingType(getBindingType());
		return clone;
	}
	
	/**
	 * A readable string representation for debugging.
	 */
	public String toString() {
		if(bindingType.equals(BindingType.NONE)) {
			return String.format("%s=%s",getName(),value==null?"null":value.toString());
		}
		else {
			return String.format("%s (%s)=%s",getName(),getBindingType().name(),(binding==null?"null":binding));
		}
	}

	/**
	 * Update a value based on a push notification. Note that this
	 * triggers any change listeners on this property. These
	 * notifications are currently NOT on the UI thread.
	 */
	@Override
	public void valueChange(QualifiedValue val) {
		log.tracef("%s(%d).valueChange %s now %s",TAG,hashCode(),getName(),val.getValue().toString());
		if( val!=null && val.getValue()!=null) {
			setValue(val.getValue());
		}
	}
}
