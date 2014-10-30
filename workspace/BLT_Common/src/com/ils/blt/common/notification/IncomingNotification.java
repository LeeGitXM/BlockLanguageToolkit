/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;



import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.Connection;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
/**
 * This class is used to hold value change information coming from the "engine" representing 
 * a new value arriving at an input port. Depending on the type of connector, the class of the
 * value object is one of the following:
 *    - Signal
 *    - TruthValue
 *    - Double
 *    - String
 * These are always bound into a QualifiedValue in order to propagate quality information.
 * 
 * This is a property container with no behavior.
 */
public class IncomingNotification {
	private final Connection connection;
	private final QualifiedValue value;
	
	/**
	 * Constructor. This version is used by the tag listener to notify
	 *        listeners with TAG_READ binding that have no connection.
	 *        Value is expressed as an object.
	 * 
	 * @param val the new value
	 */
	public IncomingNotification(QualifiedValue val)  {	
		this.connection = null;
		this.value = val;
	}
	/**
	 * Constructor. Value is expressed as an object.
	 * 
	 * @param cxn the connection that is the source or target of the value.
	 *              Usage depends on the direction of the information exchange.
	 * @param val the new value
	 */
	public IncomingNotification(Connection cxn, QualifiedValue val)  {	
		this.connection = cxn;
		this.value = val;
	}
	
	public Connection getConnection()    { return connection; }
	/**
	 * Convert the value to a qualified value. If null, generate
	 * a qualified value of BAD quality.
	 * @return the value cast to a QualifiedValue
	 */
	public QualifiedValue getValue() { return value;}
	/**
	 * Convert the value to a signal. If null, return
	 * an empty signal.
	 * @return the value cast to a signal
	 */
	public Signal getValueAsSignal() {
		Signal result = null;
		if( value instanceof Signal ) result = (Signal)value;
		else result = new Signal("","","");
		return result; 
	}
	/**
	 * Convert the value to a truth value. If null, return
	 * a UNKNOWN state.
	 * @return the value cast to a TruthValue
	 */
	public TruthValue getValueAsTruthValue() {
		TruthValue result = TruthValue.UNKNOWN;
		Object obj = value.getValue();
		if( obj!=null) {
			if( obj instanceof TruthValue ) result = (TruthValue)obj;
			else {
				String tv = obj.toString().toUpperCase();
				try {
					result = TruthValue.valueOf(tv);
				}
				catch(IllegalArgumentException iae) {}
			}
		}
		return result; 
	}
}
