/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;

/**
 * A SQLWriter writes the value of its input to a database table.
 */
@ExecutableBlock
public class SQL extends AbstractProcessBlock implements ProcessBlock {
	protected static String BLOCK_PROPERTY_DB = "DB";
	protected static String BLOCK_PROPERTY_SQL = "SQL";
	
	protected String sql = "";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SQL() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public SQL(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("SQL Writer");
		BlockProperty db = new BlockProperty(BLOCK_PROPERTY_DB,"",PropertyType.STRING,true);
		properties.put(BLOCK_PROPERTY_DB, db);
		BlockProperty sqlprop = new BlockProperty(BLOCK_PROPERTY_SQL,"",PropertyType.STRING,true);
		properties.put(BLOCK_PROPERTY_SQL, sqlprop);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
	}
	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/sql.png");
		prototype.setPaletteLabel("SQL");
		prototype.setTooltipText("Write incoming value to a database");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setIconPath("Block/icons/embedded/database.png");
		view.setPreferredHeight(60);   // Size of the block plus inset
		view.setPreferredWidth(48);
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.ICON);
	}
}