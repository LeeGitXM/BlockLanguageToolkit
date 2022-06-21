/**
 *   (c) 2013-2021  ILS Automation. All rights reserved.
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

import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;


/**
 * Display a panel to show connection attributes.    
 */

public class ConnectionPropertyEditor extends AbstractPropertyEditor {
	private static final long serialVersionUID = 8971626415423709616L;
	private final static String CLSS = "ConnectionPropertyEditor";
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private Connection cxn;
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackageName());
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
	 * Un-subscribe to notifications to allow cleanup. 
	 */
	public void shutdown() {
	}
	/** 
	 * For a connection we get all our information from the UI.
	 * The attribute list is fixed. 
	 */
	private void init() {
		setLayout(new MigLayout("flowy,ins 2"));

		
		JPanel corePanel = new CorePropertyPanel(cxn);
		add(corePanel,"grow,push");
	
		// Upstream
		BlockPanel panel = new BlockPanel(BlockPanel.UPSTREAM,(ProcessBlockView)cxn.getOrigin().getBlock(),
				((BasicAnchorPoint)cxn.getOrigin()).getLastValue());
		add(panel,"grow,push");
		String key = NotificationKey.keyForConnection(cxn.getOrigin().getBlock().getId().toString(), cxn.getOrigin().getId().toString());
		log.infof("%s.init: connection %s = %s",CLSS,cxn.getOrigin().getBlock().toString(),key);
		notificationHandler.addNotificationChangeListener(key,CLSS,panel);
		// Downstream
		// NOTE: Only the upstream anchor is populated with "last value". 
		panel = new BlockPanel(BlockPanel.DOWNSTREAM,(ProcessBlockView)cxn.getTerminus().getBlock(),
				((BasicAnchorPoint)cxn.getOrigin()).getLastValue());
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
	private class BlockPanel extends JPanel implements NotificationChangeListener {
		private static final String DOWNSTREAM = "Downstream";
		private static final String UPSTREAM = "Upstream";
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		private JTextField textField = null;
		public BlockPanel(String heading,ProcessBlockView blk,QualifiedValue qv) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,heading);
			
			add(createLabel("Label"),"skip");
			add(createTextField(blk.getName()),"span,growx");
			add(createLabel("Class"),"skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
			if( heading.equals(DOWNSTREAM) ) {
				add(createLabel("LastValue"),"skip");
				this.textField = createTextField((qv==null?"":qv.getValue().toString()));
				add(textField,"span,growx");
			}
		}
		
		// =============================== NotificationChangeListener ================================
		// All we care about is the value
		@Override
		public void diagramStateChange(String path, String state) {}

		@Override
		public void bindingChange(String name, String binding) {}

		@Override
		public void nameChange(String name) {}

		@Override
		public void propertyChange(String name, Object value) {}

		@Override
		public void valueChange(QualifiedValue value) {
			if( textField!=null) textField.setText(value.getValue().toString());
		}

		@Override
		public void watermarkChange(String newWatermark) {}
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


