/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.WindowConstants;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

import org.w3c.dom.Document;

import com.ils.jgx.editor.JgxWorkspaceEditor;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.mxgraph.io.mxCodec;
import com.mxgraph.model.mxIGraphModel;
import com.mxgraph.swing.util.mxSwingConstants;
import com.mxgraph.util.mxConstants;
import com.mxgraph.util.mxXmlUtils;

/**
 * A Diagnostics frame is a internal frame  designed to hold a model diagram. 
 */
public class DiagnosticsFrame extends JInternalFrame implements ResourceWorkspaceFrame {
	private static final long serialVersionUID = 4617016159408932941L;
	private static final String TAG = "DiagnosticsFrame:";
	private static final int DEFAULT_WIDTH = 700;
	private static final int DEFAULT_HEIGHT = 200;
	private final JgxWorkspaceEditor editor;
	private final JComponent contentPane;
	private final mxIGraphModel model;
	private final DesignerContext context;
	private final String treePath;    // Tree path in nav tree
	private long resourceId = -1;
	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	/**
	 * 
	 * @param mdl model resource that describes the edge/vertex layout
	 * @param name title/key of the diagram
	 */
	public DiagnosticsFrame(DesignerContext ctx,String name,String path) {
		super(name,true,true,false,false);  // resizable,closable,maximizable,iconifiable
		this.context = ctx;
		this.treePath = path;
		setName(name);
		this.contentPane = (JComponent)getContentPane();
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		
		// Border
	    Border compoundBorder = BorderFactory.createCompoundBorder(
	    		BorderFactory.createRaisedBevelBorder(), 
	    		BorderFactory.createLoweredBevelBorder());
	    Border emptyBorder = new EmptyBorder(3,3,3,3);   // Insets: top, left, bottom,right
	    compoundBorder = BorderFactory.createCompoundBorder( emptyBorder,compoundBorder);
	    contentPane.setBorder(compoundBorder);
	    
	    // Put the graphics editor in this frame
        mxSwingConstants.SHADOW_COLOR = Color.LIGHT_GRAY;
        mxConstants.W3C_SHADOWCOLOR = "#D3D3D3";

        PropertiesRequestHandler handler = new PropertiesRequestHandler(context,treePath);
        editor = new JgxWorkspaceEditor(handler);
        editor.setPreferredSize(new Dimension(DEFAULT_WIDTH,DEFAULT_HEIGHT));
        contentPane.add(editor,BorderLayout.CENTER);
        editor.updateTitle();
        this.model = editor.getGraphComponent().getGraph().getModel();
        
        pack();
        setLocation(0,0);  // Initial location. We can be moved.
	}
	
	public String getModel() {
		// Creates the URL-encoded XML data
		mxCodec codec = new mxCodec();
		String xml="ERROR-ENCODING EXCEPTION";
		try {
			xml = URLEncoder.encode(mxXmlUtils.getXml(codec.encode(model)), "UTF-8");
		}
		catch(UnsupportedEncodingException uee) {
			log.error(TAG+"getModel - encoding exception ("+uee.getMessage()+")");
		}
		return xml;
	}
	/**
	 * Setting the model involves a re-display of the diagram.
	 * @param mdl
	 */
	public void setModel(String xml) {
		try {
			Document document = mxXmlUtils.parseXml(URLDecoder.decode(xml, "UTF-8"));
			mxCodec codec = new mxCodec(document);
			codec.decode(document.getDocumentElement(),model);
			editor.setModified(false);
			editor.getUndoManager().clear();
			editor.getGraphComponent().zoomAndCenter();
		}
		catch(UnsupportedEncodingException uee) {
			log.error(TAG+"setModel - decoding exception ("+uee.getMessage()+")");
		}
	}
	
	@Override
	public String getKey() { return getName(); }
	
	@Override
	public boolean isInitiallyVisible() {
		return true;
	}
	
	public void setResourceId(long resid) { 
		log.info(String.format("%s: setResourceId,%d", TAG,resid));
		this.resourceId = resid; 
	}
	
	public void saveResource() {
		byte[] data = getModel().getBytes();
		context.updateResource(resourceId,data);
	}

}
