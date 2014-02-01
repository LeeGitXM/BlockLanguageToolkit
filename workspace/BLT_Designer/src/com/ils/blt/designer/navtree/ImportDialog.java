package com.ils.blt.designer.navtree;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

public class ImportDialog extends JDialog implements ActionListener {
	private final static String TAG = "ImportDialog";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static String FILE_CHOOSER_NAME = "FileChoser";
	private final static String TEXT_FIELD_NAME = "TextField";
	private static final long serialVersionUID = 8813971334526492335L;
	private final int HEIGHT = 80;
	private final int WIDTH = 400;
	private File filePath = null;
	private JFileChooser fc;
	private JTextField diagramNameField;
	private String diagramName = "";
	private final LoggerEx log;
	
	// Doing nothing works quite well.
	public ImportDialog(String name) {
		super();
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(WIDTH,HEIGHT));
        this.diagramName = name;
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}

	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		String columnConstraints = "";
		String layoutConstraints = "filly,ins 10";
		String rowConstraints = "";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		
		//Create a file chooser
	    fc = new JFileChooser();
	    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
	    FileNameExtensionFilter filter = new FileNameExtensionFilter("JSON exports", "json", "txt");
	    fc.setFileFilter(filter);
	    
	    fc.setDialogTitle(BundleUtil.get().getString(PREFIX+".Import.DialogTitle"));
	    fc.setApproveButtonText(BundleUtil.get().getString(PREFIX+".Import.ApproveButton"));
	    fc.setEnabled(false);
	    fc.setMultiSelectionEnabled(false);
	    fc.setName(FILE_CHOOSER_NAME);
	    fc.addActionListener(this);
	    
		columnConstraints = "";
		layoutConstraints = "fillx,ins 10";
		rowConstraints = "";
	    JPanel namePanel = new JPanel(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
	    JLabel label = new JLabel(BundleUtil.get().getString(PREFIX+".Import.NameLabel"));
	    namePanel.add(label, "skip");
	    diagramNameField = new JTextField(diagramName);
	    diagramNameField.setName(TEXT_FIELD_NAME);
	    diagramNameField.setColumns(30);
	    diagramNameField.addActionListener(this);
	    namePanel.add(diagramNameField, "growx,wrap");
	    
	    add(namePanel, "wrap");
	    add(fc, "wrap");
	}
	
	/**
	 * @return the name that the user entered.
	 */
	public String getDiagramName() { return diagramName; }
	/**
	 * @return the file path that the user selected from the chooser.
	 */
	public File getFilePath() { return filePath; }
	
	/**
	 * We receive events from the file chooser.
	 * For the text field, the command is the field contents.
	 */
	public void actionPerformed(ActionEvent e) {
		log.infof("%s: actionPerformed %s = %s", TAG,e.getActionCommand(),((JComponent)(e.getSource())).getName());
		if( e.getActionCommand().equals(JFileChooser.APPROVE_SELECTION)) {
			//filePath = fc.getSelectedFile();
		}
		else {
			//filePath =null;
		};
		//this.dispose();
	}
}
