/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.common.control;

import java.util.UUID;

import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.ConnectionPostNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This is a rudimentary implementation of the Execution Controller interface. 
 *  It's function is as a stand-in for the true block execution controller in
 *  other environments separate from the Gateway. It does nothing but log warnings.
 */
public class BasicExecutionController implements ExecutionController  {
	protected static final String TAG = "BasicExecutionController";

	protected static final LoggerEx log = LogUtil.getLogger(BasicExecutionController.class.getPackage().getName());

	public void acceptBroadcastNotification(BroadcastNotification note){
		log.warnf("%s.acceptBroadcastNotification: WARNING: Executed from stub class",TAG);
	}
	public void acceptCompletionNotification(OutgoingNotification note){
		log.warnf("%s.acceptCompletionNotification: WARNING: Executed from stub class",TAG);
	}
	public void acceptConnectionPostNotification(ConnectionPostNotification note){
		log.warnf("%s.acceptConnectionPostNotification: WARNING: Executed from stub class",TAG);
	}
	public void alterSubscription(UUID diagramId,UUID id,String propertyName){
		log.warnf("%s.alterSubscription: WARNING: Executed from stub class",TAG);
	}
	public void clearSubscriptions(){
		log.warnf("%s.clearSubscriptions: WARNING: Executed from stub class",TAG);
	}
	public void sendPropertyNotification(String id, String propertyName, QualifiedValue val){
		log.warnf("%s.sendPropertyNotification: WARNING: Executed from stub class",TAG);
	}
	public void pet(Watchdog dog){
		log.warnf("%s.pet: WARNING: Executed from stub class",TAG);
	}
	public void removeWatchdog(Watchdog dog){
		log.warnf("%s.removeWatchdog: WARNING: Executed from stub class",TAG);
	}
	public void updateTag(UUID diagramId,String path,QualifiedValue val) {
		log.warnf("%s.updateTag: WARNING: Executed from stub class",TAG);
	}
	@Override
	public void acceptBroadcastNotification(
			com.ils.blt.common.control.BroadcastNotification note) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void acceptCompletionNotification(
			com.ils.blt.common.control.OutgoingNotification note) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void acceptConnectionPostNotification(
			com.ils.blt.common.control.ConnectionPostNotification note) {
		// TODO Auto-generated method stub
		
	}
}
