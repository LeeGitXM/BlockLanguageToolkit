/**
 *   (c) 2012-2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.Hashtable;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ils.block.NewValueNotification;
import com.ils.block.ProcessBlock;
import com.ils.connection.Connection;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * The diagram model analyzes the XML document that is the "model" extracted from a
 * a JGraphX diagram. It converts the XML into block and connection objects. 
 * This provides answers to questions that the model control may ask about "what's next?".  
 * 
 *  The document is constant for the life of this instance.
 */
public class DiagramModel {
	
	private static String TAG = "DiagramModel";
	private final LoggerEx log;
	private final Document doc;
	private boolean valid = false;
	private final long projectId;
	private final long resourceId;
	private final Hashtable<String,ProcessBlock> blocks;        // Key by block number
	private final Hashtable<String,Connection> connections;   // Key by source:port
	
	
	/**
	 * Create a model that encapsulates an XML document describing a diagram.
	 * @param doc an XML DOM document. 
	 */
	public DiagramModel(Document dom,long proj,long res) { 
		this.doc = dom;
		this.projectId = proj;
		this.resourceId = res;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		blocks = new Hashtable<String,ProcessBlock>();
		connections = new Hashtable<String,Connection>();
		analyze();
	}
	
	/**
	 * Analyze the DOM for nodes.
	 */
	private void analyze() {
		log.debugf("%s: analyze ....%d:%d",TAG,projectId,resourceId);
		Node root = doc.getDocumentElement();
		if( root!=null && root.getNodeName().equalsIgnoreCase("mxGraphModel")) {
			// The next node is a gratuitous root
			root = root.getFirstChild();
			if( root!=null && root.getNodeType()==Node.ELEMENT_NODE) {
				NodeList cells = ((Element)root).getElementsByTagName("mxCell");
				Node node = null;
				Element cell = null;
				for( int index=0;index<cells.getLength();index++) {
					node = cells.item(index);
					if( node.getNodeType()==Node.ELEMENT_NODE) {
						cell = (Element)node;
						if( !cell.getAttribute("vertex").isEmpty()) {
							String id = cell.getAttribute("id");
							if( !id.isEmpty() ) {
								log.debugf("%s: analyze adding block %s",TAG,id);
								ProcessBlock block = blockFromElement(cell);
								if( block!=null ) {
									blocks.put(id, block);
									log.debugf("%s: analyze added  block %s",TAG,id);
								}
							}
						}
						else if( !cell.getAttribute("edge").isEmpty()) {
							String source = cell.getAttribute("source");
							if( !source.isEmpty() ) {
								log.debugf("%s: analyze adding connection from %s",TAG,source);
								Connection cxn = connectionFromElement(cell);
								if( cxn!=null) {
									String key = String.format("%s:%s",source,cxn.getUpstreamPortName());
									connections.put(key, cxn);
								}
							}
						}
					}
				}
			}
 			
		}
		else {
			log.warn(TAG+"analyze: Unexpected root element ("+(root==null?"null":root.getNodeName())+")");
		}
	}
	
	/**
	 * The subject block has just placed a new result on an output port. Determine which input block
	 * and port, if any, is connected to the output. There is, at most one. A null return indicates no downstream connection.
	 * @param new value notification of an incoming change
	 * @return a new value notification for the downstream block
	 */
	public NewValueNotification getValueUpdate(NewValueNotification incoming) {
		ProcessBlock block = incoming.getBlock();
		String port = incoming.getPort();
		QualifiedValue value = incoming.getValue();
		
		NewValueNotification nvn = null;
		String key = String.format("%s:%s",String.valueOf(block.getBlockId()),port);
		Connection cxn = connections.get(key);
		if( cxn!=null ) {
			ProcessBlock blk = blocks.get(cxn.getSource());
			if( blk!=null) {
				nvn = new NewValueNotification(blk,cxn.getUpstreamPortName(),value);
			}	
		}
		
		return nvn;
	}
	
	/**
	 * Report on whether or not the DOM contained more than one connected node.
	 */
	public boolean isValid() { return valid; }
	
	/**
	 * Convert a "vertx" element into a block
	 */
	private ProcessBlock blockFromElement(Element element) {
		return null;
	}
	/**
	 * Convert a "edge" element into a connection
	 */
	private Connection connectionFromElement(Element element) {
		return null;
	}
}
