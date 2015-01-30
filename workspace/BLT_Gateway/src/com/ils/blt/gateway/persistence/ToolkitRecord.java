/**
 *   (c) 2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.persistence;


import com.inductiveautomation.ignition.gateway.localdb.persistence.PersistentRecord;
import com.inductiveautomation.ignition.gateway.localdb.persistence.RecordMeta;
import com.inductiveautomation.ignition.gateway.localdb.persistence.StringField;
import simpleorm.dataset.SFieldFlags;

/**
 * Save and access toolkit-wide properties in the HSQL persistent database.
 * For documentation relating to the SimpleORM data model:
 * @See: http://simpleorm.org/sorm/whitepaper.html
 */
public class ToolkitRecord extends PersistentRecord {
	public static final String TABLE_NAME = "ILS_Toolkit_Properties";
	
	public static final RecordMeta<ToolkitRecord> META = new RecordMeta<>(ToolkitRecord.class, TABLE_NAME);
	static SFieldFlags[] primary = {SFieldFlags.SPRIMARY_KEY,SFieldFlags.SMANDATORY};
	static SFieldFlags[] secondary = {SFieldFlags.SMANDATORY};
	public static final StringField Name = new StringField(META, "Name",primary );
	public static final StringField Value = new StringField(META, "Value",secondary).setDefault("");
	
	public RecordMeta<?> getMeta() {return META; }
	
	public String getName() { return getString(Name); }
	public String getValue() { return getString(Value); }
	public void setName(String str) { setString(Name,str); }
	public void setValue(String str) { setString(Value,str); }
}
