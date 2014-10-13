/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Iterator;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.block.common.FixedSizeQueue;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.HysteresisType;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class compares input against a set limit value.
 * Return true if m of n are above.
 */
@ExecutableBlock
public class HighLimitSampleCount extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "HighValuePattern";
	private TruthValue truthValue = TruthValue.UNSET;
	private double limit;
	private final static int DEFAULT_BUFFER_SIZE = 1;
	private final FixedSizeQueue<QualifiedValue> queue;
	private double deadband = 0;
	private HysteresisType hysteresis = HysteresisType.NEVER;
	private int sampleSize = DEFAULT_BUFFER_SIZE;
	private boolean fillRequired = true;
	private int triggerCount = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public HighLimitSampleCount() {
		queue = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "limit".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public HighLimitSampleCount(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		queue = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
		initialize();
	}
	
	
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("HighLimitSample");
		BlockProperty bp = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LIMIT,new Double(limit),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_LIMIT, bp);
		BlockProperty fillProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED,new Boolean(fillRequired),PropertyType.BOOLEAN,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED, fillProperty);
		BlockProperty sizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Integer(sampleSize),PropertyType.INTEGER,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sizeProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT,new Integer(triggerCount),PropertyType.INTEGER,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT, triggerProperty);
		BlockProperty deadbandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND,new Double(deadband),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_DEADBAND, deadbandProperty);
		BlockProperty hProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HYSTERESIS,hysteresis,PropertyType.HYSTERESIS,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_HYSTERESIS, hProperty);

		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		truthValue = TruthValue.UNSET;
	}
	
	/**
	 * A new value has arrived. Add it to the queue.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) ) {
			QualifiedValue qv = vcn.getValue();
			log.infof("%s.acceptValue: Received %s",TAG,qv.getValue().toString());
			if( qv.getQuality().isGood() ) {
				queue.add(qv);
				if( queue.size() >= sampleSize || !fillRequired) {
					TruthValue result = checkPassConditions(truthValue);
					if( !isLocked() ) {
						// Give it a new timestamp
						QualifiedValue outval = new BasicQualifiedValue(result);
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
						controller.acceptCompletionNotification(nvn);
					}
					// Even if locked, we update the current state
					truthValue = result;
				}
				else {
					QualifiedValue outval = new BasicQualifiedValue(TruthValue.UNKNOWN);
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
					controller.acceptCompletionNotification(nvn);
				}
			}
			else {
				// Post bad value on output, clear queue
				if( !isLocked() ) {
					QualifiedValue outval = new BasicQualifiedValue(new Double(Double.NaN),qv.getQuality(),qv.getTimestamp());
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
					controller.acceptCompletionNotification(nvn);
				}
				queue.clear();
			}
		}
	}
	
	/**
	 * Handle a limit or sample size change.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();

		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_LIMIT)) {
			try {
				limit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_DEADBAND)) {
			try {
				deadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert deadband to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED)) {
			fillRequired = fcns.coerceToBoolean(event.getNewValue().toString());
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_HYSTERESIS)) {
			try {
				hysteresis = HysteresisType.valueOf(event.getNewValue().toString().toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.propertyChange: Unable to convert hysteresis (%s)",TAG,iae.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE) ) {
			// Trigger an evaluation
			try {
				int val = Integer.parseInt(event.getNewValue().toString());
				if( val>0 ) {
					sampleSize = val;
					queue.clear();
					queue.setBufferSize(sampleSize);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT) ) {
			// Trigger an evaluation
			try {
				triggerCount = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert trigger count to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	/**
	 *  When unlocking, set the remembered state as "UNSET". This will allow
	 *  the next value to generate output, no matter what.
	 */
	@Override
	public void setLocked(boolean flag) {
		if(this.locked && !flag ) {
			truthValue = TruthValue.UNSET;
		}
		this.locked = flag;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/high_limit_sample.png");
		prototype.setPaletteLabel("HighLimit(n)");
		prototype.setTooltipText("Return true if a specified count of points in a sample is above a limit");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/xbarn_gt.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	
	/**
	 * Check pass-fail criteria, presumably because of a new input.
	 * The queue never contains bad values. 
	 */
	private TruthValue checkPassConditions(TruthValue current) {
		TruthValue result = TruthValue.UNSET;
		double threshold = limit;
		switch(hysteresis) {
		case TRUE:
			if( current.equals(TruthValue.TRUE)) threshold = limit - deadband;
			else threshold = limit;
			break;
		case FALSE:
			if( current.equals(TruthValue.TRUE)) threshold = limit;
			else threshold = limit + deadband;
			break;
		case ALWAYS:
			if( current.equals(TruthValue.TRUE)) threshold = limit - deadband;
			else threshold = limit + deadband;
			break;
		case NEVER:
		default:
			threshold = limit;  
		}
		int count = 0;
		Iterator<QualifiedValue> walker = queue.iterator();
		while( walker.hasNext() ) {
			QualifiedValue qv = walker.next();
			if( qv.getQuality().isGood() ) {
				double val = 0.0;
				try {
					val = Double.parseDouble(qv.getValue().toString());
					if( val>threshold ) count++;
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s:checkPassConditions detected not-a-number in queue (%s), ignored",TAG,nfe.getLocalizedMessage());
					continue;
				}
			};
		}

		if( count>=triggerCount ) result = TruthValue.TRUE;
		else result = TruthValue.FALSE;
		return result;
	}
}