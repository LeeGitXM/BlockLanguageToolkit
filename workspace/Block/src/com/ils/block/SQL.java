/**
 *   (c) 2014-2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.common.db.DBUtility;
import com.inductiveautomation.ignition.gateway.SRContext;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * A SQLWriter writes the value of its input to a database table.
 */
@ExecutableBlock 
public class SQL extends AbstractProcessBlock implements ProcessBlock {
	protected static String BLOCK_PROPERTY_SQL = "SQL";
	
	protected String sql = "";
	private final DBUtility dbUtility;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SQL() {
		initialize();
		initializePrototype();
		GatewayContext context = SRContext.get();
		dbUtility = new DBUtility(context);
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
		GatewayContext context = SRContext.get();
		dbUtility = new DBUtility(context);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("SQL Writer");
		
		BlockProperty sqlprop = new BlockProperty(BLOCK_PROPERTY_SQL,sql,PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_SQL, sqlprop);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
	}
	/**
	 * A new value has appeared on our input. Execute the SQL statement as a prepared statement
	 * with the input as the sole argument. There is no output port.
	 * 
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		if(!isLocked() ) {
			String arg = vcn.getValue().toString();
			//log.infof("%s.acceptValue: %s", getName(),qv.getValue().toString());
			UUID pid = getParentId();
			DiagnosticDiagram diagram = controller.getDiagram(pid.toString());
			String source = controller.getProductionDatabase();
			if( diagram.getState().equals(DiagramState.ISOLATED) ) source = controller.getIsolationDatabase();
			dbUtility.runPreparedStatement(sql, arg, source, null);
		}
	}
	
	/**
	 * Handle a change to one of our custom properties.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_SQL)) {
			sql = event.getNewValue().toString();
		}
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