/**
 *   (c) 2020  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.apache.commons.math3.stat.descriptive.moment.GeometricMean;
import org.apache.commons.math3.stat.descriptive.moment.Kurtosis;
import org.apache.commons.math3.stat.descriptive.moment.Mean;
import org.apache.commons.math3.stat.descriptive.moment.SecondMoment;
import org.apache.commons.math3.stat.descriptive.moment.Skewness;
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation;
import org.apache.commons.math3.stat.descriptive.moment.Variance;
import org.apache.commons.math3.stat.descriptive.rank.Max;
import org.apache.commons.math3.stat.descriptive.rank.Median;
import org.apache.commons.math3.stat.descriptive.rank.Min;
import org.apache.commons.math3.stat.descriptive.summary.Product;
import org.apache.commons.math3.stat.descriptive.summary.Sum;
import org.apache.commons.math3.stat.descriptive.summary.SumOfLogs;
import org.apache.commons.math3.stat.descriptive.summary.SumOfSquares;

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
import com.ils.blt.common.block.StatFunction;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualityCode;

/**
 * This class computes an average of the input over a time interval.
 */
@ExecutableBlock
public class StatisticsTime extends AbstractProcessBlock implements ProcessBlock {
	private final String CLSS = "StatisticsTime";
	private final ConcurrentLinkedQueue<QualifiedValue> buffer;
	private boolean clearOnReset = false;
	QualifiedValue currentValue = null;
	private double scanInterval = 15.0;    // ~secs
	private double timeWindow = 60;        // ~ secs
	private final Watchdog dog;
	private StatFunction function = StatFunction.RANGE;
	private BlockProperty valueProperty = null;
	
    private final GeometricMean gmeanfn = new GeometricMean();
    private final Kurtosis kurtfn = new Kurtosis();
	private final Max maxfn = new Max();
	private final Mean meanfn = new Mean();
    private final Median medianfn = new Median();
    private final Min minfn = new Min();
    private final Product prodfn = new Product();
    private final SecondMoment smfn = new SecondMoment();
    private final Skewness skewfn = new Skewness();
    private final StandardDeviation sdfn = new StandardDeviation();
    private final Sum sumfn= new Sum();
    private final SumOfLogs solfn = new SumOfLogs();
    private final SumOfSquares sosfn = new SumOfSquares();
    private final Variance varfn = new Variance();
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public StatisticsTime() {
		initialize();
		buffer = new ConcurrentLinkedQueue<>();
		dog = new Watchdog(getName(),this);
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public StatisticsTime(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
		buffer = new ConcurrentLinkedQueue<>();
		
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("StatisticsTime");
		
		BlockProperty resetProperty =  new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.FALSE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, resetProperty);
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME_MINUTES,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);
		BlockProperty statProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION,function,PropertyType.STATISTICS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION, statProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Double(Double.NaN),PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define the main output.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		timer.removeWatchdog(dog);    // Stop evaluation
		if( clearOnReset ) {
			buffer.clear();
			currentValue = null;
			valueProperty.setValue(new Double(Double.NaN));
		}
	}

	@Override
	public void start() {
		super.start();
		if( !dog.isActive() && scanInterval>0.0 ) {
			dog.setSecondsDelay(scanInterval);
			timer.updateWatchdog(dog);  // pet dog
		}
	}

	@Override
	public void stop() {
		timer.removeWatchdog(dog);
	}
	
	/**
	 * A new value has arrived. Simply set the current value.
	 * (We poll for the current value on an interval).
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		QualityCode qual = qv.getQuality();
		if( qual.isGood() && qv.getValue()!=null && !qv.getValue().toString().isEmpty() ) {
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				currentValue = new BasicQualifiedValue(dbl,qv.getQuality(),qv.getTimestamp());
				log.tracef("%s.acceptValue: %s",getName(),qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue exception converting incoming %s to double (%s)",getName(),qv.getValue().toString(),nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.acceptValue received a GOOD value, but null",getName());
		}
	}
	
	/**
	 * The interval timer has expired. Evaluate the buffer.
	 * If the currentValue is null, just re-evaluate with 
	 * existing data.
	 */
	@Override
	public void evaluate() {
		// Add the currentValue to the queue
		if( currentValue!=null) buffer.add(currentValue);
		// Evaluate the buffer and report
		int maxPoints = (int)((timeWindow+0.99*scanInterval)/scanInterval);
		while(buffer.size() > maxPoints ) {  
			buffer.remove();
		}
		log.tracef("%s(%d).evaluate %d of %d points",getName(),hashCode(),buffer.size(),maxPoints);
		if( buffer.size() >= maxPoints) {
			double result = computeStatistic();
			log.tracef("%s.evaluate avg=%f",getName(),result);
			// Give it a new timestamp
			lastValue = new TestAwareQualifiedValue(timer,result);
			if( !isLocked() ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
			// Even if locked, we update the current state
			valueProperty.setValue(result);
			controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,new TestAwareQualifiedValue(timer,result));
		}

		dog.setSecondsDelay(scanInterval);
		timer.updateWatchdog(dog);  // pet dog
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}

	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s(%d).propertyChange: %s = %s",getName(),hashCode(),propertyName,event.getNewValue().toString());
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET)) {
			try {
				clearOnReset = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				double oldInterval = scanInterval;
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval < 0.1 ) scanInterval = 0.1;   // Don't allow to go too fast
				if( dog.isActive() && scanInterval < oldInterval ) {
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString());
				if( timeWindow<=0.0) timeWindow = scanInterval;
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION)) {
			try {
				function = StatFunction.valueOf(event.getNewValue().toString());
				currentValue=null;  // Prevents duplicate from being added to the buffer
				evaluate();
			}
			catch(IllegalArgumentException nfe) {
				log.warnf("%s: propertyChange Unable to convert %s to a function (%s)",CLSS,event.getNewValue().toString(),nfe.getLocalizedMessage());
			}
		}
		// Activity buffer size handled in superior method
		else if( !propertyName.equals(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE) ){
			log.warnf("%s.propertyChange:Unrecognized property (%s)",getName(),propertyName);
		}
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		log.tracef("%s(%d).getInternalStatus: buffer size = %d",getName(),hashCode(),buffer.size());
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Average", String.valueOf(currentValue));
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		Iterator<QualifiedValue> walker = buffer.iterator();
		while( walker.hasNext() ) {
			Map<String,String> qvMap = new HashMap<>();
			QualifiedValue qv = walker.next();
			qvMap.put("Value", qv.getValue().toString());
			qvMap.put("Quality", qv.getQuality().toString());
			qvMap.put("Timestamp", qv.getTimestamp().toString());
			descBuffer.add(qvMap);
		}
		return descriptor;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/statisticst.png");
		prototype.setPaletteLabel("Statistics(t)");
		prototype.setTooltipText("Compute the selected statistic at regular time intervals within the configured duration.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/statisticst.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the statistic, presumably because of a new input.
	 */
	private double computeStatistic() {
		double result = Double.NaN;
		int size = buffer.size();
		double[] values = new double[size];
		int index = 0;
		for( QualifiedValue qv:buffer) {
			values[index] = fcns.coerceToDouble(qv.getValue());
			index++;
		}
		switch(function) {
			case GEOMETRIC_MEAN:result = gmeanfn.evaluate(values); break;
			case KURTOSIS:result = kurtfn.evaluate(values); break;
			case MAXIMUM: result = maxfn.evaluate(values); break;
			case MEAN: 	  result = meanfn.evaluate(values); break;
			case MEDIAN:  result = medianfn.evaluate(values); break;
			case MINIMUM: result = minfn.evaluate(values); break;
			case PRODUCT: result = prodfn.evaluate(values); break;
			case RANGE:   result = maxfn.evaluate(values)-minfn.evaluate(values); break;      
			case SECOND_MOMENT:     result = smfn.evaluate(values); break; 
			case SKEW:    result = skewfn.evaluate(values); break; 
			case STANDARD_DEVIATION: result = sdfn.evaluate(values); break;
			case SUM:     result = sumfn.evaluate(values); break; 
			case SUM_OF_LOGS:    result = solfn.evaluate(values); break;
			case SUM_OF_SQUARES: result = sosfn.evaluate(values); break; 
			case VARIANCE: result = varfn.evaluate(values); break;
		}
		return result;	
	}
}