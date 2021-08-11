/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.prefs.Preferences;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import net.miginfocom.swing.MigLayout;
/**
 * Display a dialog that defines the file save path.
 *    ExportDialog ed = new ExportDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 */

public class SaveValidationResultsDialog extends JDialog implements ActionListener { 
	private final static String TAG = "SaveVaidationResultsDialog";
	private static final String APPROVE_BUTTON = "OK";
	private final static String DIALOG_TITLE = "Save Validation Results";
	private final static String FILE_CHOOSER_NAME = "FileChooser";
	private final static String PREF_RESULTS_DIR = "ValidationResultsDir";
	private static final long serialVersionUID = 2882399376824334427L;
	private static final int DLG_HEIGHT = 80;
	private static final int DLG_WIDTH = 400;
	private File filePath = null;
	private JFileChooser fc;
	private final LoggerEx log;
	private final Preferences prefs;
	
	
	public SaveValidationResultsDialog(JRootPane root,JComponent parent) {
		super(SwingUtilities.getWindowAncestor(root));
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(DLG_WIDTH,DLG_HEIGHT));
        setLocationRelativeTo(parent);
        setAlwaysOnTop(true);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        this.prefs = Preferences.userRoot().node(BLTProperties.PREFERENCES_NAME);
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		final String columnConstraints = "";
		final String layoutConstraints = "ins 3 10 3 10,filly";
		final String rowConstraints = "";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		//Create a file chooser
	    fc = new JFileChooser();
	    fc.setFileSelectionMode(JFileChooser.SAVE_DIALOG);
	    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
	    FileNameExtensionFilter filter = new FileNameExtensionFilter("Log output", "log", "txt");
	    fc.setFileFilter(filter);
	    String startDirectoryName = prefs.get(PREF_RESULTS_DIR,null);
	    if(startDirectoryName!=null ) {
	    	File startDirectory = new File(startDirectoryName);
		    fc.setCurrentDirectory(startDirectory);
	    }
	    
	    fc.setDialogTitle(DIALOG_TITLE);
	    fc.setDialogType(JFileChooser.SAVE_DIALOG);
	    fc.setApproveButtonText(APPROVE_BUTTON);
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
	 * Set the selected file to (presumably) the last path selected
	 */
	public void setSelectedFile(String path) {
		File file = new File(path);
		fc.setSelectedFile(file);
	}
	/**
	 * We receive events from the file chooser.
	 * For the text field, the command is the field contents.
	 */
	public void actionPerformed(ActionEvent e) {
		if( e.getActionCommand().equals(JFileChooser.APPROVE_SELECTION)) {
			filePath = fc.getSelectedFile();
			if( filePath!=null ) {
				prefs.put(PREF_RESULTS_DIR, filePath.getParent());
			}
			log.infof("%s.actionPerformed set file path to: %s (%s)",TAG,filePath.getAbsolutePath(),filePath.getParent()); 
		}
		else {
			filePath =null;
		};
		this.dispose();
	}
	
	
}
