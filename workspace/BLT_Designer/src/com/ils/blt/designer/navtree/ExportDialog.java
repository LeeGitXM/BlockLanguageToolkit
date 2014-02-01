/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
/**
 * Display a dialog to export a diagram.
 *    ExportDialog ed = new ExportDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 */

public class ExportDialog extends JDialog implements ActionListener { 
	private final static String TAG = "ExportDialog";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static String FILE_CHOOSER_NAME = "FileChoser";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int HEIGHT = 80;
	private final int WIDTH = 400;
	private File filePath = null;
	private JFileChooser fc;
	private final LoggerEx log;
	
	
	public ExportDialog() {
		super();
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(WIDTH,HEIGHT));
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		final String columnConstraints = "";
		final String layoutConstraints = "filly,ins 10";
		final String rowConstraints = "";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		//Create a file chooser
	    fc = new JFileChooser();
	    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
	    FileNameExtensionFilter filter = new FileNameExtensionFilter("JSON exports", "json", "txt");
	    fc.setFileFilter(filter);
	    
	    fc.setDialogTitle(BundleUtil.get().getString(PREFIX+".Export.DialogTitle"));
	    fc.setApproveButtonText(BundleUtil.get().getString(PREFIX+".Export.ApproveButton"));
	    fc.setEnabled(false);
	    fc.setMultiSelectionEnabled(false);
	    fc.setName(FILE_CHOOSER_NAME);
	    fc.addActionListener(this);
	    add(fc, "wrap");
	}
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
			filePath = fc.getSelectedFile();
			log.infof("%s: actionPerformed set file path to: %s",TAG,filePath.getAbsolutePath()); 
		}
		else {
			filePath =null;
		};
		this.dispose();
	}
	
	
}
