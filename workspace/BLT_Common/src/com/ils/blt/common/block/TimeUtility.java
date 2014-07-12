/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;

import java.util.concurrent.TimeUnit;


/**
 * This class presents several static utility methods surrounding time units.
 */
public class TimeUtility
{
	/**
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
	 * @return the value for a time value in seconds and specified time unit
	 */
	public static double cannonicalValueForValue(double val,TimeUnit unit) {
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
	 * @return the cannonical value for a time value and unit
	 */
	public static double valueForCannonicalValue(double val,TimeUnit unit) {
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
