/**
 *   (c) 2013-2017  ILS Automation. All rights reserved.
 */
package com.ils.blt.common;

import com.ils.blt.common.block.TruthValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
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
	private final static String TAG = "UtilityFunctions";
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
		if( !val.isEmpty() ) {
			try{
				result = Double.parseDouble(val);
			}
			catch(NumberFormatException nfe) {
				//log.error(TAG+".parseDouble: Format exception "+nfe.getLocalizedMessage(),nfe);
				log.errorf("%s.parseDouble: Format exception for value %s",TAG,val);
			}
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
		if( !val.isEmpty()) {
			try{
				result = Integer.decode(val);    // Works with hex
			}
			catch(NumberFormatException nfe) {
				double dbl = parseDouble(val);
				if( !Double.isNaN(dbl)) result = (int)dbl;
			}
		}
		return result;
	}
	
	/**
	 * Safely parse a long. Catch and report a format exception.
	 * @return  the long equivalent of the input. If the input
	 *         could not be parsed then return zero.
	 */
	public long parseLong(String val) {
		long result = 0;
		if( !val.isEmpty()) {
			try{
				result = Long.decode(val);    // Works with hex
			}
			catch(NumberFormatException nfe) {
				double dbl = parseDouble(val);
				if( !Double.isNaN(dbl)) result = (long)dbl;
			}
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
	 * Force a Double, Integer or String to a double. Reports NumberFormatException
	 * for bad input - and sets result to 0.0.
	 */
	public double coerceToDouble(Object val)  {
		double result = 0.0;
		if( val!=null ) {
			if( val instanceof Double)       result = ((Double)val).doubleValue();
			else if( val instanceof Integer) result = ((Integer)val).intValue();
			else                             result  = parseDouble(val.toString());
		}
		return result;
	}
	
	/**
	 * Force a Double, Integer or String to an int. Reports NumberFormatException
	 * for bad input - and sets result to 0.
	 */
	public int coerceToInteger(Object val) throws NumberFormatException {
		int result = 0;
		if( val !=null ) {
			if( val instanceof Integer)      result = ((Integer)val).intValue();
			else if( val instanceof Long)    result = ((Long)val).intValue();
			else if( val instanceof Double)  result = ((Double)val).intValue();
			else                             result = parseInteger(val.toString());
		}
		return result;
	}
	/**
	 * Force a Double, Integer or String to an int. Reports NumberFormatException
	 * for bad input - and sets result to 0.
	 */
	public long coerceToLong(Object val) throws NumberFormatException {
		long result = 0;
		if( val !=null ) {
			if( val instanceof Long)         result = ((Long)val).longValue();
			else if( val instanceof Integer) result = ((Integer)val).intValue();
			else if( val instanceof Double)  result = ((Double)val).longValue();
			else                             result = parseLong(val.toString());
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
	 * Force a Boolean, Double, Integer or String to a TruthValue.
	 * We wind up with a TRUE/FALSE, except for a String which can be UNKNOWN.
	 */
	public TruthValue coerceToTruthValue(Object val) {
		TruthValue result = TruthValue.UNSET;
		if( val instanceof TruthValue ) {
			result = (TruthValue)val;
		}
		else if(val instanceof String) {
			try {
				result = TruthValue.valueOf(val.toString().toUpperCase());
			}
			catch( IllegalArgumentException iae) {
				log.warnf("%s.qualifiedValueAsTruthValue: Exception converting %s (%s)", TAG,val.toString(),iae.getLocalizedMessage());
			}	
		}
		else if( val!=null ) {
			boolean flag = coerceToBoolean(val);
			result = (flag?TruthValue.TRUE:TruthValue.FALSE);
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
	 * Strip a tagpath of its provider name. 
	 * @param tagPath
	 * @return
	 */
	public String providerlessPath(String tagPath) {
		String path = tagPath;
		if( path==null ) path="";
		int pos = tagPath.indexOf("]");
		if( pos>0 ) path = tagPath.substring(pos+1);
		return path;
	}
	/**
	 * Convert the value to a qualified value. If null, generate
	 * a qualified value of BAD quality.
	 * @return the value cast to a QualifiedValue
	 */
	public QualifiedValue objectToQualifiedValue(Object value) {
		QualifiedValue result = null;
		if( value!=null ) {
			if( value instanceof QualifiedValue )        result = new BasicQualifiedValue(value);
			else if( value instanceof TruthValue ) {
				result = new BasicQualifiedValue( ((TruthValue)value).name());
			}
			else if( value instanceof Double ||
					 value instanceof Integer||
					 value instanceof String ||
					 value instanceof Boolean) {
				result = new BasicQualifiedValue( value);
			}
			else{
				result = new BasicQualifiedValue(value,new BasicQuality("unrecognized data type",Quality.Level.Bad));
			}
		}
		else {
			result = new BasicQualifiedValue("",new BasicQuality("null value",Quality.Level.Bad));
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
