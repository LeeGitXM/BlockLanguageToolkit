package com.ils.blt.designer.navtree;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.prefs.Preferences;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import net.miginfocom.swing.MigLayout;

public class ImportDialog extends JDialog implements ActionListener {
	private final static String TAG = "ImportDialog";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static String FILE_CHOOSER_NAME = "FileChoser";
	private static final long serialVersionUID = 8813971334526492335L;
	private static final int DLG_HEIGHT = 80;
	private static final int DLG_WIDTH = 400;
	private File filePath = null;
	private JFileChooser fc;
	private final String nameLabel;
	private final String title;
	private final LoggerEx log;
	private final Preferences prefs;
	
	// Doing nothing works quite well.
	public ImportDialog(Frame frame,String label,String title) {
		super(frame);
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(DLG_WIDTH,DLG_HEIGHT));
        this.nameLabel = label;
        this.title = title;
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        this.prefs = Preferences.userRoot().node(BLTProperties.PREFERENCES_NAME);
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
	    String startDirectoryName = prefs.get(BLTProperties.PREF_EXIM_DIRECTORY,System.getProperty(BLTProperties.EXIM_PATH));
	    if(startDirectoryName!=null ) {
	    	File startDirectory = new File(startDirectoryName);
		    fc.setCurrentDirectory(startDirectory);
	    }
	    
	    fc.setDialogTitle(title);
	    fc.setApproveButtonText(BundleUtil.get().getString(PREFIX+".Import.ApproveButton"));
	    fc.setEnabled(false);
	    fc.setMultiSelectionEnabled(false);
	    fc.setName(FILE_CHOOSER_NAME);
	    fc.addActionListener(this);
	    
		columnConstraints = "";
		layoutConstraints = "fillx,ins 10";
		rowConstraints = "";
	    JPanel namePanel = new JPanel(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
	    JLabel label = new JLabel(nameLabel);
	    namePanel.add(label, "skip");    
	    add(namePanel, "wrap");
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
			if( filePath!=null ) {
				String fileName = filePath.getName();
				if(fileName.indexOf(".")<0) {
					filePath = new File(filePath.getAbsolutePath()+".json");
				}
				prefs.put(BLTProperties.PREF_EXIM_DIRECTORY, filePath.getParent());
				this.dispose();
			}
		}
		else if(e.getActionCommand().equals(JFileChooser.CANCEL_SELECTION)){
			filePath =null;
			this.dispose();
		}
	}
}
