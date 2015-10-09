/*
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.common.control;

import java.util.UUID;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.ConnectionPostNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;


/**
 *  This interface describes a controller that accepts change notifications
 *  from the blocks and acts as a delegate for facilities that are not
 *  within the Block Definition project. 
 */
public interface ExecutionController  {

	public void acceptBroadcastNotification(BroadcastNotification note);
	public void acceptCompletionNotification(OutgoingNotification note);
	public void acceptConnectionPostNotification(ConnectionPostNotification note);
	public void alterSubscription(UUID diagramId,UUID id,String propertyName);
	public void clearCache();
	public void clearSubscriptions();
	public String getIsolationDatabase();
	public String getIsolationProvider();
	public String getProductionDatabase();
	public String getProductionProvider();
	public double getIsolationTimeFactor();
	public boolean hasActiveSubscription(ProcessBlock block,BlockProperty property);
	public QualifiedValue getTagValue(UUID diagramId,String path);
	public void sendPropertyNotification(String id, String propertyName, QualifiedValue val);
	public void sendConnectionNotification(String blockid, String port, QualifiedValue val);
	public void sendStateNotification(String diagramid, String val);
	public void updateTag(UUID diagramId,String path,QualifiedValue val);
	public String validateTag(UUID diagramId,String tagPath); 
}
