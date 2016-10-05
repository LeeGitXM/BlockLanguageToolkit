/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common;

import java.util.concurrent.TimeUnit;


/**
 * This class presents several static utility methods surrounding time units.
 */
public class TimeUtility
{
	/**
	 * @param unit the time unit
	 * @return a short-form abbreviation for the specified unit.
	 */
	public static String abbreviationForUnit(TimeUnit unit) {
		String result = "S";
		switch(unit) {
			case MILLISECONDS:
				result = "MS";
				break;
			case MINUTES:
				result = "M";
				break;
			case HOURS:
				result = "H";
				break;
			case DAYS:
				result = "D";
				break;
			case SECONDS:
			default:
			
		}
		return result;
	}
	/**
	 * @param val time interval expressed in the units provided
	 * @param unit the time unit
	 * @return a value ~ secs for a time value in the specified time unit
	 */
	public static double canonicalValueForValue(double val,TimeUnit unit) {
		double result = val;
		switch(unit) {
			case MILLISECONDS:
				result = val/1000.;
				break;
			case MINUTES:
				result = val*60.;
				break;
			case HOURS:
				result = val*3600.;
				break;
			case DAYS:
				result = val*24*3600.;
				break;
			case SECONDS:
			default:
			
		}
		return result;

	}
	/**
	 * @param val time interval expressed in seconds
	 * @param unit the time unit
	 * @return the a time value in the specified units for an original value ~seconds
	 */
	public static double valueForCanonicalValue(double val,TimeUnit unit) {
		double result = val;
		switch(unit) {
			case MILLISECONDS:
				result = val*1000.;
				break;
			case MINUTES:
				result = val/60.;
				break;
			case HOURS:
				result = val/3600.;
				break;
			case DAYS:
				result = val/(24*3600.);
				break;
			case SECONDS:
			default:
			
		}
		return result;
	}
	/**
	 * @param time the interval (in seconds) for which we want an appropriate unit
	 * @return  the time unit appropriate to the value.
	 * 			The value is in seconds.
	 */
	public static TimeUnit unitForValue(double time) {
		TimeUnit result = TimeUnit.SECONDS;
		if( time > 0.0 && time < 1.0 ) result = TimeUnit.MILLISECONDS;
		else if( time > 24*3599. ) result = TimeUnit.DAYS;
		else if( time > 3599. ) result = TimeUnit.HOURS;
		else if( time > 59.0 ) result = TimeUnit.MINUTES;
		
		return result;
	}
}
