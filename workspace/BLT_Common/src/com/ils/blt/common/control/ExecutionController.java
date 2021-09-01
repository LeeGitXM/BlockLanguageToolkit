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
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;


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
	public DiagnosticDiagram getDiagram(ProjectResourceId diagramId);
	public String getIsolationDatabase();
	public String getIsolationProvider();
	public String getProductionDatabase();
	public ProcessBlock getProcessBlock(ProjectResourceId diagramId,String blockId);
	public String getProductionProvider();
	public double getIsolationTimeFactor();
	public String getSubscribedPath(ProcessBlock block,BlockProperty property);
	public boolean hasActiveSubscription(ProcessBlock block,BlockProperty property,String tagPath);
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(ProjectResourceId diagramId,String blockId,String portName);
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(ProjectResourceId diagramId,UUID blockId,boolean spanDiagrams);
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(ProjectResourceId diagramId,UUID blockId,boolean spanDiagrams);
	public List<SerializableBlockStateDescriptor> listSinksForSource(ProjectResourceId diagramId,String blockName);
	public List<SerializableBlockStateDescriptor> listSourcesForSink(ProjectResourceId diagramId,String blockName);
	public QualifiedValue getTagValue(ProjectResourceId diagramId,String path);
	public void sendAlertNotification(ProjectResourceId resid, String val);
	public void sendAuxDataNotification(String id,QualifiedValue val);
	public void sendConnectionNotification(String blockid, String port, QualifiedValue val);
	public void sendNameChangeNotification(String blockid, String name);
	public void sendPropertyBindingNotification(String blockid, String propertyName, String val);
	public void sendPropertyNotification(String blockid, String propertyName, QualifiedValue val);
	public void sendStateNotification(ProjectResourceId resid, String val);
	public void sendWatermarkNotification(ProjectResourceId diagramid, String val);
	public void updateTag(ProjectResourceId diagramId,String path,QualifiedValue val);
	public String validateTag(ProjectResourceId diagramId,String tagPath);
}
