/*
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.common.control;

import java.util.List;
import java.util.UUID;

import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.ConnectionPostNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
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
	public void clearCache();
	public void clearSubscriptions();
	public DiagnosticDiagram getDiagram(String diagramId);
	public String getIsolationDatabase();
	public String getIsolationProvider();
	public String getProductionDatabase();
	public ProcessBlock getProcessBlock(String diagramId,String blockId);
	public String getProductionProvider();
	public double getIsolationTimeFactor();
	public String getSubscribedPath(ProcessBlock block,BlockProperty property);
	public boolean hasActiveSubscription(ProcessBlock block,BlockProperty property,String tagPath);
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName);
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams);
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams);
	public List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName);
	public List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockName);
	public QualifiedValue getTagValue(UUID diagramId,String path);
	public void sendAlertNotification(long resid, String val);
	public void sendConnectionNotification(String blockid, String port, QualifiedValue val);
	public void sendNameChangeNotification(String blockid, String name);
	public void sendPropertyBindingNotification(String id, String propertyName, String val);
	public void sendPropertyNotification(String id, String propertyName, QualifiedValue val);
	public void sendStateNotification(long resid, String val);
	public void sendWatermarkNotification(String diagramid, String val);
	public void updateTag(UUID diagramId,String path,QualifiedValue val);
	public String validateTag(UUID diagramId,String tagPath);
	void sendPropertyUpdateNotification(OutgoingNotification note, String blockId); 
}
