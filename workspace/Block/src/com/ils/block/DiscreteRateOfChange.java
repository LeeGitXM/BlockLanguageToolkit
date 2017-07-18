/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class DiscreteRateOfChange extends AbstractProcessBlock implements ProcessBlock {
	
	private final static String BLOCK_PROPERTY_LINEAR_INTERPOLATION = "LinearInterpolation";
	private final static String BLOCK_PROPERTY_POLYNOMIAL_ORDER = "PolynomialOrder";
	private final static String BLOCK_PROPERTY_SAMPLE_TYPE  = "SampleType";
	private final static String BLOCK_PROPERTY_SCALE_FACTOR = "ScaleFactor";
	private final static String BLOCK_PROPERTY_UPDATE_SIZE  = "UpdateSize";
	private final static String BLOCK_PROPERTY_UPDATE_TYPE  = "UpdateType";
	
	private boolean clearOnReset = true;
	private boolean fillRequired = true;
	private boolean linearInterpolation = true;
	private int polynomialOrder = 2;
	private int sampleSize = 1;
	private String sampleType = "";
	private double scaleFactor = 1.0;
	private int updateSize = 1;
	private String updateType = "";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DiscreteRateOfChange() {
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
	public DiscreteRateOfChange(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("DiscreteRateOfChange");
		
		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty fillProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED, fillProperty);
		BlockProperty liProperty = new BlockProperty(BLOCK_PROPERTY_LINEAR_INTERPOLATION,new Boolean(linearInterpolation),PropertyType.BOOLEAN,true);
		setProperty(BLOCK_PROPERTY_LINEAR_INTERPOLATION, liProperty);
		BlockProperty poProperty = new BlockProperty(BLOCK_PROPERTY_POLYNOMIAL_ORDER,new Integer(polynomialOrder),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_POLYNOMIAL_ORDER, poProperty);
		
		BlockProperty sampleSizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Integer(sampleSize),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sampleSizeProperty);
		BlockProperty sampleTypeProperty = new BlockProperty(BLOCK_PROPERTY_SAMPLE_TYPE,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_SAMPLE_TYPE, sampleTypeProperty);
		BlockProperty sfProperty = new BlockProperty(BLOCK_PROPERTY_SCALE_FACTOR,new Double(scaleFactor),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_SCALE_FACTOR, sfProperty);
		BlockProperty updateSizeProperty = new BlockProperty(BLOCK_PROPERTY_UPDATE_SIZE,new Integer(updateSize),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_UPDATE_SIZE, updateSizeProperty);
		BlockProperty updateTypeProperty = new BlockProperty(BLOCK_PROPERTY_UPDATE_TYPE,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_UPDATE_TYPE, updateTypeProperty);
		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	

	/**
	 * A new value has appeared on our input.  Pass it on.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		if(!isLocked() ) {
			lastValue = vcn.getValue();
			//log.infof("%s.acceptValue: %s", getName(),qv.getValue().toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	/**
	 * Handle a change to one of our custom properties.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET)) {
			try {
				clearOnReset = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED)) {
			try {
				fillRequired = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert fill flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if( propertyName.equalsIgnoreCase(BLOCK_PROPERTY_LINEAR_INTERPOLATION)) {
			try {
				linearInterpolation = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert linear interpolation flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_POLYNOMIAL_ORDER)) {
			try {
				polynomialOrder = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert order number to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE)) {
			try {
				sampleSize = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_SAMPLE_TYPE)) {
			sampleType = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_SCALE_FACTOR)) {
			try {
				scaleFactor = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert scale factor to an number (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_UPDATE_SIZE)) {
			try {
				updateSize = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert update size to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_UPDATE_SIZE)) {
			updateType = event.getNewValue().toString();
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		updateStateForNewValue(qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/todo.png");
		prototype.setPaletteLabel("DiscreteChange");
		prototype.setTooltipText("Pass through");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.JUNCTION);
		desc.setPreferredHeight(32);
		desc.setPreferredWidth(32);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_MUSTARD);
		desc.setCtypeEditable(true);
	}
}