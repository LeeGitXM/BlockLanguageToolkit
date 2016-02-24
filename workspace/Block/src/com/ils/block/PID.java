/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 *   Code based on sample code at: 
 *        http://www.codeproject.com/Articles/36459/PID-process-control-a-Cruise-Control-example
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
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class applies PID control to its input. Algorithm taken from Wikipedia.
 */
@ExecutableBlock
public class PID extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "PID";
	protected static String BLOCK_PROPERTY_KD = "Kd";
	protected static String BLOCK_PROPERTY_KI = "Ki";
	protected static String BLOCK_PROPERTY_KP = "Kp";
	protected static String BLOCK_PROPERTY_INITIAL_VALUE  = "InitialValue";
	protected static String BLOCK_PROPERTY_SET_POINT      = "SetPoint";
	protected static String SETPOINT_PORT      = "setpoint";
	protected static String PROPORTIONAL_PORT      = "p";
	protected static String INTEGRAL_PORT          = "i";
	protected static String DERIVATIVE_PORT        = "d";
	private double kd = Double.NaN;
	private double ki = Double.NaN;
	private double kp = Double.NaN;
	private double error = 0.0;
	private double initialValue = Double.NaN;
	private double integral = 0.0;
	private double interval = 10.0;  // secs
	private double pv = Double.NaN;
	private double setPoint = Double.NaN;
	private final Watchdog dog;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public PID() {
		dog = new Watchdog(TAG,this);
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public PID(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	@Override
	public void reset() {
		super.reset();
		error = 0.0;
		integral = 0.0;
		pv = initialValue;
		log.infof("%s.reset",TAG);
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
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("PID");
		this.isReceiver = true;
		BlockProperty pvProperty = new BlockProperty(BLOCK_PROPERTY_INITIAL_VALUE,new Double(pv),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_INITIAL_VALUE, pvProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);
		BlockProperty kdProperty = new BlockProperty(BLOCK_PROPERTY_KD,new Double(kd),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_KD, kdProperty);
		BlockProperty kiProperty = new BlockProperty(BLOCK_PROPERTY_KI,new Double(ki),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_KI, kiProperty);
		BlockProperty kpProperty = new BlockProperty(BLOCK_PROPERTY_KP,new Double(kp),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_KP, kpProperty);
		
		
		// Define a two inputs -- feedback and setpoint
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("V");
		anchors.add(input);
		
		AnchorPrototype setpoint = new AnchorPrototype(SETPOINT_PORT,AnchorDirection.INCOMING,ConnectionType.DATA);
		setpoint.setAnnotation("S");
		anchors.add(setpoint);

		// Define outputs for the result, plus components of the error
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.B);
		anchors.add(output);
		output = new AnchorPrototype(PROPORTIONAL_PORT,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("P");
		anchors.add(output);
		output = new AnchorPrototype(INTEGRAL_PORT,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("I");
		anchors.add(output);
		output = new AnchorPrototype(DERIVATIVE_PORT,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("D");
		anchors.add(output);
	}

	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BLOCK_PROPERTY_KD)) {
			try {
				kd = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert kd value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_KI)) {
			try {
				ki = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert ki value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_KP)) {
			try {
				kp = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert kp value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BLOCK_PROPERTY_INITIAL_VALUE)) {
			try {
				initialValue = Double.parseDouble(event.getNewValue().toString());
				pv = initialValue;
				log.infof("%s.propertyChange: initial value now %f (%s)",TAG,initialValue,getBlockId().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert initial value to an float (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * We record the value and start the watchdog timer.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME)  ) {
			QualifiedValue qv = vcn.getValue();
			log.tracef("%s.acceptValue: port %s value = %s ",TAG,port,qv.getValue().toString());
			try {
				pv = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming data to double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( port.equals(SETPOINT_PORT)  ) {
			QualifiedValue qv = vcn.getValue();
			try {
				setPoint = Double.parseDouble(qv.getValue().toString());
				if( !dog.isActive() ) {
					dog.setSecondsDelay(interval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming data to double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	
	/**
	 * We're received a transmitted signal. Deal with it, if appropriate.
	 * At a later time, we may implement pattern filtering or some other
	 * method to filter out unwanted messages. For now, if we recognize the command,
	 * then execute it.
	 * 
	 * @param sn signal notification.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal signal = sn.getSignal();
		log.tracef("%s.acceptValue: signal = %s (%s)",TAG,signal.getCommand(),getBlockId().toString());
		if( signal.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_RESET)) {
			reset();
		}
		else if( signal.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_START)) {
			if(!dog.isActive()) {
				error = 0.0;
				integral = 0.0;
				pv = initialValue;
				evaluate();
			}
		}
	}
	/**
	 * The interval has expired. Reset interval, then compute output.
	 * Do not compute anything until all parameters have been set.
	 */
	@Override
	public synchronized void evaluate() {
		dog.setSecondsDelay(interval);
		timer.updateWatchdog(dog);  // pet dog
		if( !isValid() ) return;
		// Compute PID
		double previousError = error;
		double dt = interval;        // In seconds
		error = setPoint - pv;
		integral += error*dt;
		double derivative = (error - previousError)/dt;
		double proportionalContribution = kp*error;
		double integralContribution = ki*integral;
		double derivativeContribution = kd*derivative;
		double result = proportionalContribution + integralContribution + derivativeContribution;
		if( log.isTraceEnabled() ) {
			log.infof("%s.evaluate setpoint= %f, pv = %f, error = %f, previous error = %f",TAG,setPoint,pv,error,previousError);
			log.infof("%s.evaluate Kp = %f",TAG,proportionalContribution);
			log.infof("%s.evaluate Ki = %f",TAG,integralContribution);
			log.infof("%s.evaluate Kd = %f",TAG,derivativeContribution);
		}
		
		
		log.tracef("%s: evaluate - pid out is %f",TAG,result);
		QualifiedValue ans = new TestAwareQualifiedValue(timer,result);
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,ans);
		controller.acceptCompletionNotification(nvn);
		QualifiedValue prop = new TestAwareQualifiedValue(timer,proportionalContribution);
		nvn = new OutgoingNotification(this,PROPORTIONAL_PORT,prop);
		controller.acceptCompletionNotification(nvn);
		QualifiedValue integ = new TestAwareQualifiedValue(timer,integralContribution);
		nvn = new OutgoingNotification(this,INTEGRAL_PORT,integ);
		controller.acceptCompletionNotification(nvn);
		QualifiedValue deriv = new TestAwareQualifiedValue(timer,derivativeContribution);
		nvn = new OutgoingNotification(this,DERIVATIVE_PORT,deriv);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(ans,prop,integ,deriv);		
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv,QualifiedValue prop,QualifiedValue integ,QualifiedValue derivative) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
		controller.sendConnectionNotification(getBlockId().toString(), PROPORTIONAL_PORT, prop);
		controller.sendConnectionNotification(getBlockId().toString(), INTEGRAL_PORT, integ);
		controller.sendConnectionNotification(getBlockId().toString(), DERIVATIVE_PORT, derivative);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/PID.png");
		prototype.setPaletteLabel("PID");
		prototype.setTooltipText("Perform PID control based on the input and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("PID");
		desc.setEmbeddedFontSize(24);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setReceiveEnabled(true);
	}
	
	// Check that all parameters have been set.
	private boolean isValid() {
		boolean result = false;
		if( interval>0 &&
			!Double.isNaN(kd) &&
			!Double.isNaN(ki) &&
			!Double.isNaN(kp) &&
			!Double.isNaN(pv) &&
			!Double.isNaN(setPoint) ) result = true;
		if(!result) log.warnf("%s.isValid: %s invalid (%s)",TAG,
				(interval<=0?"interval":
				(Double.isNaN(kd)?"kd":
				(Double.isNaN(ki)?"ki":
				(Double.isNaN(kp)?"kp":
				(Double.isNaN(pv)?"pv":"setpoint"))))),getBlockId().toString());
		return result;
	}
}