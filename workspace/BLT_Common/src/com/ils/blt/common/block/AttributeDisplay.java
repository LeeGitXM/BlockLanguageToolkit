/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;

import java.io.Serializable;
import java.util.UUID;

/**
 * Designer equivalents of these display a single property of a block.
 * In the Gateway these are simply "beans" and hold the few attributes.
 * These are serializable and therefore can be instantiated in any scope.
 */
public class AttributeDisplay implements Cloneable,Serializable {
	private static final long serialVersionUID = -1802235810478552377L;
	private static final String CLSS = "AttributeDisplay";
	public static final int DEFAULT_WIDTH = 120;
	public static final int DEFAULT_HEIGHT = 15;
	private String blockId;
	private String propertyName;
	private UUID uuid;
	private int offsetX;  // X distance relative to the block
	private int offsetY;  // Y distance relative to the block
	private int preferredHeight = DEFAULT_HEIGHT;   // Size the view to "natural" size
	private int preferredWidth  = DEFAULT_WIDTH;    // Size the view to "natural" siz
	private int x = 0;  	  // horizontal position on screen
	private int y = 0;  	  // vertical position on screen
	private int height = DEFAULT_HEIGHT; 
	private int width  = DEFAULT_WIDTH;
	
	/**
	 * Constructor: No-arg constructor used for JSON deserialization
	 */
	public AttributeDisplay() {
		this.uuid = UUID.randomUUID();
	}
	/**
	 * Constructor: 
	 */
	public AttributeDisplay(String id,String name) {
		this.blockId = id;
		this.propertyName = name;
		this.uuid = UUID.randomUUID();
	}
	
	public String getBlockId() { return this.blockId; }
	public String getPropertyName() { return this.propertyName; }
	public int getPreferredHeight() { return this.preferredHeight; }
	public int getPreferredWidth() { return this.preferredWidth; }
	public int getX() { return this.x; }
	public void setX(int pos) { this.x = pos; }
	public int getY() { return this.y; }
	public void setY(int pos) { this.y = pos; }
	public UUID getUUID() { return this.uuid; }
	
	@Override
	public AttributeDisplay clone() {
		AttributeDisplay ad = new AttributeDisplay(this.blockId,this.propertyName);
		ad.preferredHeight = getPreferredHeight();
		ad.preferredWidth = getPreferredWidth();
		ad.x = this.x;
		ad.y = this.y;
		ad.uuid = this.uuid;
		return ad;
	} 
	/**
	 * Start listening on property change
	 */
	public void start() {
		
	}
}