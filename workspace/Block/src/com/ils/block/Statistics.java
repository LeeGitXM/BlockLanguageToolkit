/**
 *   (c) 2020  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

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
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
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
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

/**
 * This class performs a statistical calculation from the most recent values on 
 * each of its potential multiple inputs.
 */
@ExecutableBlock
public class Statistics extends AbstractProcessBlock implements ProcessBlock {
	private final String CLSS = "Statistics";
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> qualifiedValueMap;
	private final boolean DEBUG = true;
	private final Watchdog dog;
	private StatFunction function = StatFunction.RANGE;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
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
	public Statistics() {
		initialize();
		initializePrototype();
		dog = new Watchdog(getName(),this);
		qualifiedValueMap = new HashMap<>();
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Statistics(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
		qualifiedValueMap = new HashMap<>();
	}


	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Statistics");
		BlockProperty statProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION,function,PropertyType.STATISTICS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION, statProperty);
		
		// Define a single input, but it can support multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(true);
		anchors.add(input);

		// Define the main output, a data value.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}

	/**
	 * Initialize the qualified value map.
	 */
	@Override
	public void start() {
		super.start();
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,qualifiedValueMap,Double.NaN);
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		timer.removeWatchdog(dog);	
	}

	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * For now we simply record the change in the map and start the watchdog. 
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		
		QualifiedValue qv = incoming.getValue();
		if( qv!=null && qv.getValue()!=null ) {
			String key = incoming.getConnection().getSource().toString();
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				qv = new BasicQualifiedValue(dbl,qv.getQuality(),qv.getTimestamp());
				dog.setSecondsDelay(synchInterval);
				if(DEBUG) log.infof("%s.acceptValue got %s for %s", getName(),dbl.toString(),key);
				timer.updateWatchdog(dog);  // pet dog
				
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",getName(),nfe.getLocalizedMessage());
				qv = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
			qualifiedValueMap.put(key, qv);
			recordActivity(Activity.ACTIVITY_RECEIVE,key,qv.getValue().toString());
		}
		else {
			log.warnf("%s.acceptValue: received null value",getName());
		}
	}
	/**
	 * The coalescing time has expired. Place the sum of all inputs on the output.
	 */
	@Override
	public void evaluate() {
		if(DEBUG) log.infof("%s.evaluate ...", getName());
		if( !isLocked() && !qualifiedValueMap.isEmpty()) {
			double value = getAggregateResult();
			if(DEBUG) log.infof("%s.evaluate ... value = %3.2f", getName(),value);
			lastValue = new TestAwareQualifiedValue(timer,new Double(value),getAggregateQuality());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	
	/**
	 * Compute the overall statistic, presumably because of a new input.
	 * The datatype of the QualifiedValue is guaranteed to be a Double.
	 */
	private double getAggregateResult() {
		double result = Double.NaN;
		int size = qualifiedValueMap.size();
		double[] values = new double[size];
		int index = 0;
		for( QualifiedValue qv:qualifiedValueMap.values()) {
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
	
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION)) {
			try {
				function = StatFunction.valueOf(event.getNewValue().toString());
				reset();
			}
			catch(IllegalArgumentException nfe) {
				log.warnf("%s: propertyChange Unable to convert %s to a function (%s)",CLSS,event.getNewValue().toString(),nfe.getLocalizedMessage());
			}
		}	
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
		notifyOfStatus(qv);
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		for(String key:qualifiedValueMap.keySet()) {
			QualifiedValue qv = (QualifiedValue)qualifiedValueMap.get(key);
			if( qv!=null && qv.getValue()!=null) {
				attributes.put(key, String.valueOf(qv.getValue()));
			}
			else {
				attributes.put(key,"NULL"); 
			}
		}
		return descriptor;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/statistics.png");
		prototype.setPaletteLabel("Statistics");
		prototype.setTooltipText("Compute a selected statistic across all inputs and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/statistics.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the overall quality.
	 * NOTE: This is only valid if the current state is UNKNOWN.
	 */
	private Quality getAggregateQuality() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		Quality q = DataQuality.GOOD_DATA; 
		for(QualifiedValue qv:values) {
			if( !qv.getQuality().isGood() ) return qv.getQuality();
		}
		return q;	
	}
}