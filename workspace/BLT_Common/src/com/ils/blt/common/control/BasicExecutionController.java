/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.blt.common.control;

import java.util.ArrayList;
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
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This is a rudimentary implementation of the Execution Controller interface. 
 *  It's function is as a stand-in for the true block execution controller in
 *  other environments separate from the Gateway. It does nothing but log warnings.
 */
public class BasicExecutionController implements ExecutionController  {
	protected static final String CLSS = "BasicExecutionController";

	protected static final LoggerEx log = LogUtil.getLogger(BasicExecutionController.class.getPackage().getName());

	@Override
	public void acceptBroadcastNotification(BroadcastNotification note){
		log.warnf("%s.acceptBroadcastNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void acceptCompletionNotification(OutgoingNotification note){
		log.warnf("%s.acceptCompletionNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void acceptConnectionPostNotification(ConnectionPostNotification note){
		log.warnf("%s.acceptConnectionPostNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public DiagnosticDiagram getDiagram(ProjectResourceId diagramId) {
		log.warnf("%s.getDiagram: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public void clearSubscriptions(String projectName){
		log.warnf("%s.clearSubscriptions: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public String getProjectIsolationDatabase(String projectName) {
		log.warnf("%s.getIsolationDatabase: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public String getProjectIsolationProvider(String projectName) {
		log.warnf("%s.getIsolationProvider: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public String getProjectProductionDatabase(String projectName) {
		log.warnf("%s.getProductionDatabase: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public String getProjectProductionProvider(String projectName) {
		log.warnf("%s.getProductionProvider: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public double getIsolationTimeFactor() {
		log.warnf("%s.getIsolationTimeFactor: WARNING: Executed from stub class",CLSS);
		return 0;
	}
	@Override
	public ProcessBlock getProcessBlock(ProjectResourceId diagramId,String blockId) {
		log.warnf("%s.getProcessBlock: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public String getSubscribedPath(ProcessBlock block,BlockProperty property) {
		log.warnf("%s.getSubscribedPath: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override	
	public QualifiedValue getTagValue(ProjectResourceId diagramId,String path) {
		log.warnf("%s.getTagValue: WARNING: Executed from stub class",CLSS);
		return null;
	}
	@Override
	public boolean hasActiveSubscription(ProcessBlock block,BlockProperty property,String tagPath) {
		log.warnf("%s.hasActiveSubscription: WARNING: Executed from stub class",CLSS);
		return false;
	}
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(ProjectResourceId diagramId,String blockId,String portName) {
		log.warnf("%s.listBlocksConnectedAtPort: WARNING: Executed from stub class",CLSS);
		return new ArrayList<>();
	}
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(ProjectResourceId diagramId,UUID blockId,boolean spanDiagrams) {
		log.warnf("%s.listBlocksDownstreamOf: WARNING: Executed from stub class",CLSS);
		return new ArrayList<>();
	}
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(ProjectResourceId diagramId,UUID blockId,boolean spanDiagrams) {
		log.warnf("%s.listBlocksUpstreamOf: WARNING: Executed from stub class",CLSS);
		return new ArrayList<>();
	}
	@Override
	public List<SerializableBlockStateDescriptor> listSinksForSource(ProjectResourceId diagramId,String blockName) {
		log.warnf("%s.listSinksForSource: WARNING: Executed from stub class",CLSS);
		return new ArrayList<>();
	}
	@Override
	public List<SerializableBlockStateDescriptor> listSourcesForSink(ProjectResourceId diagramId,String blockName) {
		log.warnf("%s.listSourcesForSink: WARNING: Executed from stub class",CLSS);
		return new ArrayList<>();
	}
	@Override
	public void sendAlertNotification(ProjectResourceId resid, String val) {
		log.warnf("%s.sendAlertNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendAuxDataNotification(String id,QualifiedValue val) {
		log.warnf("%s.sendAuxDataNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendConnectionNotification(String blockid, String port, QualifiedValue val) {
		log.warnf("%s.sendConnectionNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendNameChangeNotification(String blockid, String name) {
		log.warnf("%s.sendNameChangeNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendPropertyBindingNotification(String id, String propertyName, String val){
		log.warnf("%s.sendPropertyBindingNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendPropertyNotification(String id, String propertyName, QualifiedValue val){
		log.warnf("%s.sendPropertyNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendStateNotification(ProjectResourceId resourceid, String val){
		log.warnf("%s.sendStateNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void sendWatermarkNotification(ProjectResourceId diagramid, String val) {
		log.warnf("%s.sendWatermarkNotification: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public void updateTag(ProjectResourceId diagramId,String path,QualifiedValue val) {
		log.warnf("%s.updateTag: WARNING: Executed from stub class",CLSS);
	}
	@Override
	public String validateTag(ProjectResourceId diagramId,String tagPath) {
		log.warnf("%s.validateTag: WARNING: Executed from stub class",CLSS);
		return null;
	}
}
