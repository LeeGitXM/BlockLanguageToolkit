/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Display a panel to show connection attributes.    
 */

public class ConnectionPropertyEditor extends JPanel {
	private static final long serialVersionUID = 8971626415423709616L;
	private Connection cxn;
	private static final List<String> coreAttributeNames;
	
	// These are the attributes handled in the CorePropertyPanel
	static {
		coreAttributeNames = new ArrayList<String>();
		coreAttributeNames.add("class");
	}
	
	/*
	 * @param cxn the connection that we are editing 
	 */
	public ConnectionPropertyEditor(DesignerContext ctx,Connection connection) {
		this.cxn = connection;
        init();    
	}

	
	/** 
	 * For a connection we get all our information from the UI.
	 * The attribute list is fixed. 
	 */
	private void init() {
		setLayout(new MigLayout("flowy,ins 2"));

		
		JPanel panel = new CorePropertyPanel(cxn);
		add(panel,"grow,push");
	
		// Upstream
		panel = new BlockPanel("Upstream",(ProcessBlockView)cxn.getOrigin().getBlock(),
				((BasicAnchorPoint)cxn.getOrigin()).getLastValue());
		add(panel,"grow,push");
		// Downstream
		panel = new BlockPanel("Downstream",(ProcessBlockView)cxn.getTerminus().getBlock(),
				((BasicAnchorPoint)cxn.getTerminus()).getLastValue());
		add(panel,"grow,push");
	}
	
	/**
	 * Add a separator to a panel using Mig layout
	 */
	private void addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
        JLabel label = new JLabel(text);
        label.setFont(new Font("Tahoma", Font.PLAIN, 11));
        label.setForeground(Color.BLUE);
        panel.add(label, "split 2,span");
        panel.add(separator, "growx,wrap");
	}
	
	/**
	 * Create a new label
	 */
	private JLabel createLabel(String text) {
		return new JLabel(text);
	}
	
	/**
	 * Create a text field for read-only values
	 */
	private JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setEditable(false);
		return field;
	}
	
	
	/**
	 * Create a combo box for data types
	 */
	private JComboBox<String> createConnectionTypeCombo(final ConnectionType type) {
		String[] entries = new String[ConnectionType.values().length];
		int index=0;
		for(ConnectionType ctype : ConnectionType.values()) {
			entries[index]=ctype.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		if( type!=null ) box.setSelectedItem(type.toString());
		box.setEditable(false);
		box.setEnabled(false);
		return box;
	}
	
	/**
	 * A block panel is a display for properties of the upstream and downstream blocks.
	 */
	@SuppressWarnings("serial")
	private class BlockPanel extends JPanel {
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		public BlockPanel(String heading,ProcessBlockView blk,QualifiedValue qv) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,heading);
			
			add(createLabel("Label"),"skip");
			add(createTextField(blk.getName()),"span,growx");
			add(createLabel("Class"),"skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
			add(createLabel("LastValue"),"skip");
			add(createTextField((qv==null?"":qv.getValue().toString())),"span,growx");
		}
	}
	
	/**
	 * The single core property is the connection type.
	 */
	@SuppressWarnings("serial")
	private class CorePropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		
		public CorePropertyPanel(Connection connection) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Connection");
			
			add(createLabel("Type"),"skip");
			add(createConnectionTypeCombo(((BasicAnchorPoint)cxn.getOrigin()).getConnectionType()),"skip");
		}
	}
}


