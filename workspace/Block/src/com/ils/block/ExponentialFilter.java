/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import org.apache.commons.math3.analysis.function.Exp;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class filters its input with an exponentially weighted 
 * moving average.
 */
@ExecutableBlock
public class ExponentialFilter extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "ExponentialFilter";

	private double window = 60;    // ~ secs. One minute
	private BlockProperty valueProperty = null;
	private double value = Double.NaN;
	private long lastUpdateTime = Long.MIN_VALUE;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public ExponentialFilter() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "TimeWindow".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public ExponentialFilter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Exponential Filter");
		BlockProperty constant = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(window),PropertyType.TIME_MINUTES,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, constant);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		value = Double.NaN;
		lastUpdateTime = Long.MIN_VALUE;
	}
	
	
	/**
	 * A new value has appeared on an input anchor. Smooth it exponentially and place it on the
	 * output.
	 * 
	 * Exponentially smooth values. The filter constant is the time-difference
     * between measurements divided by a time constant. The longer the time difference,
     * the more that we favor the current measurement. Evaluation interval and time window
     * must have the same units.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv!=null && qv.getValue()!=null && window>0.0 ) {
			try {
				double newValue = Double.parseDouble(qv.getValue().toString());
				if( lastUpdateTime != Long.MIN_VALUE ) {
					long interval = System.currentTimeMillis() - lastUpdateTime;  // ~ msecs
					double prior = value;
					Exp exp = new Exp();
					value = prior* exp.value(-1.0*interval/(window*1000.)) + newValue*(1.0 - exp.value(-1.0*interval/(window*1000.)));
				}
				else {
					value = newValue;
				}
				lastUpdateTime = System.currentTimeMillis();
				lastValue = new BasicQualifiedValue(value,qv.getQuality(),qv.getTimestamp());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
				lastValue = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
		}
		else if( window<= 0.0 ){
			lastValue = new BasicQualifiedValue(Double.NaN,new BasicQuality(String.format("Illegal time window %f",window),Quality.Level.Bad),qv.getTimestamp());
		}
		else {
			lastValue = new BasicQualifiedValue(Double.NaN,qual,qv.getTimestamp());
		}
		if( !isLocked() ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	
	/**
	 * Handle a change to the filter value
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW) ) {
			try {
				window = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert filter value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,value);
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/exponential_filter.png");
		prototype.setPaletteLabel("Filter");
		prototype.setTooltipText("Smooth incoming values using an exponential filter");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/exponential_decay.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}