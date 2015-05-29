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
	@Override
	public void clearCache() {
		log.warnf("%s.clearCache: WARNING: Executed from stub class",TAG);
	}
	public void clearSubscriptions(){
		log.warnf("%s.clearSubscriptions: WARNING: Executed from stub class",TAG);
	}
	public String getIsolationDatabase() {
		log.warnf("%s.getIsolationDatabase: WARNING: Executed from stub class",TAG);
		return null;
	}
	public String getIsolationProvider() {
		log.warnf("%s.getIsolationProvider: WARNING: Executed from stub class",TAG);
		return null;
	}
	public String getProductionDatabase() {
		log.warnf("%s.getProductionDatabase: WARNING: Executed from stub class",TAG);
		return null;
	}
	public String getProductionProvider() {
		log.warnf("%s.getProductionProvider: WARNING: Executed from stub class",TAG);
		return null;
	}
	public double getIsolationTimeFactor() {
		log.warnf("%s.getIsolationTimeFactor: WARNING: Executed from stub class",TAG);
		return 0;
	}
	public QualifiedValue getTagValue(UUID diagramId,String path) {
		log.warnf("%s.getTagValue: WARNING: Executed from stub class",TAG);
		return null;
	}
	public void sendConnectionNotification(String blockid, String port, QualifiedValue val) {
		log.warnf("%s.sendConnectionNotification: WARNING: Executed from stub class",TAG);
	}
	public void sendPropertyNotification(String id, String propertyName, QualifiedValue val){
		log.warnf("%s.sendPropertyNotification: WARNING: Executed from stub class",TAG);
	}
	public void sendStateNotification(String id, String val){
		log.warnf("%s.sendStateNotification: WARNING: Executed from stub class",TAG);
	}
	public void updateTag(UUID diagramId,String path,QualifiedValue val) {
		log.warnf("%s.updateTag: WARNING: Executed from stub class",TAG);
	}
	public boolean validateTag(UUID diagramId,String tagPath) {
		log.warnf("%s.validateTag: WARNING: Executed from stub class",TAG);
		return false;
	}

}
