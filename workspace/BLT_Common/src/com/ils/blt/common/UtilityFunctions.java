/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.common;

import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.serializable.SerializableQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  This is a utility class of, hopefully, generally useful data conversion
 *  functions. This class carries no state.
 */
public class UtilityFunctions  {
	private final static String TAG = "UtilityFunctions: ";
	private final LoggerEx log;
	/**
	 * No-argument constructor. 
	 */
	public UtilityFunctions() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	
	/**
	 * Safely parse a double. Catch and report a format exception.
	 * @return the double equivalent of the input. If the input
	 *         could not be parsed then return Double.NaN. 
	 */
	public double parseDouble(String val) {
		double result = Double.NaN;
		try{
			result = Double.parseDouble(val);
		}
		catch(NumberFormatException nfe) {
			log.error(TAG+"parseDouble: Format exception "+nfe.getLocalizedMessage(),nfe);    // Prints stack trace
		}
		return result;
	}
	
	/**
	 * Safely parse an int. Catch and report a format exception.
	 * @return  the integer equivalent of the input. If the input
	 *         could not be parsed then return zero.
	 */
	public int parseInteger(String val) {
		int result = 0;
		try{
			result = Integer.parseInt(val);
		}
		catch(NumberFormatException nfe) {
			double dbl = parseDouble(val);
			if( !Double.isNaN(dbl)) result = (int)dbl;
		}
		return result;
	}
	/**
	 * Force a Double, Integer or String to a boolean.
	 */
	public boolean coerceToBoolean(Object val) {
		boolean result = false;
		if( val!=null ) {
			if( val instanceof Boolean)      result = ((Boolean)val).booleanValue();
			else if( val instanceof Double)  result = (((Double)val).doubleValue()!=0.0);
			else if( val instanceof Integer) result = (((Integer)val).intValue() != 0);
			else                             result = val.toString().equalsIgnoreCase("true");
		}
		return result;
	}
	
	/**
	 * Force a Double, Integer or String to a double. Throws NumberFormatException
	 * for bad input - and sets result to 0.0.
	 */
	public double coerceToDouble(Object val) {
		double result = 0.0;
		if( val!=null ) {
			if( val instanceof Double)       result = ((Double)val).doubleValue();
			else if( val instanceof Integer) result = ((Integer)val).intValue();
			else                             result  = parseDouble(val.toString());	
		}
		return result;
	}
	
	/**
	 * Force a Double, Integer or String to an int. Throws NumberFormatException
	 * for bad input - and sets result to 0.
	 */
	public int coerceToInteger(Object val) {
		int result = 0;
		if( val !=null ) {
			if( val instanceof Integer)      result = ((Integer)val).intValue();
			else if( val instanceof Double)  result = ((Double)val).intValue();
			else                             result = parseInteger(val.toString());
		}
		return result;
	}
	/**
	 * Force a Double, Integer or String to a String. 
	 * If the object is a QualifiedValue, then coerce its value's value.
	 * Guarantee the return is not null. 
	 */
	public String coerceToString(Object val) {
		String result = "";
		if(val!=null && val instanceof QualifiedValue ) {
			QualifiedValue qv = (QualifiedValue)val;
			val = qv.getValue();
		}
		if( val!=null ) {
			result = val.toString();
		}
		return result;
	}
	
	/**
	 * Determine the object type of the incoming value. If a qualified value,
	 * the extract the value portion.
	 * 
	 * TODO: Apply proper format per data type.
	 * 
	 * @return the value formatted as a String
	 */
	public String formatResult(String format,Object value) {
		String result = "";
		if(value!=null && value instanceof QualifiedValue ) {
			value = ((QualifiedValue)value).getValue();
		}
		if( value!=null ) {
			result = String.format(format, value.toString());
		}
		return result; 
	}
	
	
	/**
	 * Convert the value to a qualified value. If null, generate
	 * a qualified value of BAD quality.
	 * @return the value cast to a QualifiedValue
	 */
	public SerializableQualifiedValue objectToQualifiedValue(Object value) {
		SerializableQualifiedValue result = null;
		if( value!=null ) {
			if( value instanceof SerializableQualifiedValue ) result = (SerializableQualifiedValue)value;
			else if( value instanceof QualifiedValue )        result = new SerializableQualifiedValue(value);
			else if( value instanceof TruthValue ) {
				result = new SerializableQualifiedValue( ((TruthValue)value).name());
			}
			else if( value instanceof Double ||
					 value instanceof Integer||
					 value instanceof String ||
					 value instanceof Boolean) {
				result = new SerializableQualifiedValue( value);
			}
			else{
				result = new SerializableQualifiedValue(value,new BasicQuality("unrecognized data type",Quality.Level.Bad));
			}
		}
		else {
			result = new SerializableQualifiedValue("",new BasicQuality("null value",Quality.Level.Bad));
		}
		return result; 
	}
	/**
	 * Convert a qualified value to a truth value. If null, return
	 * a UNKNOWN state.
	 * @return the value cast to a TruthValue
	 */
	public TruthValue qualifiedValueAsTruthValue(QualifiedValue qv) {
		TruthValue result = TruthValue.UNKNOWN;
		if( qv!=null) {
			if( qv.getValue()!=null ) {
				if( qv.getValue() instanceof TruthValue ) {
					result = (TruthValue)(qv.getValue());
				}
				else {
					String val = qv.getValue().toString().toUpperCase();
					try {
						result = TruthValue.valueOf(val);
					}
					catch(IllegalArgumentException iae) {}
				}
			}
		}
		return result; 
	}
}
