package com.ils.blt.designer.navtree;

import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.NodeStatusManager;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.client.util.gui.ValidatedTextField;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourceNamingException;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.gui.CommonUI;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.project.DesignableProject;

import net.miginfocom.swing.MigLayout;

public class ImportDialog extends JDialog implements ActionListener {
	private final static String CLSS = "ImportDialog";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static String FILE_CHOOSER_NAME = "FileChoser";
  	private final static String POPUP_TITLE = "Import Diagram";
	private static final long serialVersionUID = 8813971334526492335L;
	private static final int DLG_HEIGHT = 80;
	private static final int DLG_WIDTH = 400;
	private final DesignerContext context;
	private File filePath = null;
	private ValidatedTextField diagramName;
	private final List<String> existingNames;
	private final ResourcePath parent;
	private JFileChooser fc;
	private final String buttonLabel;
	private final String fieldLabel;
	private final String title;
	private final LoggerEx log;
	private final Preferences prefs;
	private final Consumer<ProjectResourceBuilder> builderConsumer;
	private final Consumer<ProjectResourceId> onAfterCreated;
	
	// Doing nothing works quite well.
	public ImportDialog(final DesignerContext ctx,final ResourcePath rp, Consumer<ProjectResourceBuilder> builderConsumer,String buttonLabel, String fieldLabel,String title,
									AbstractNavTreeNode parent,Consumer<ProjectResourceId> onAfterCreated) {
		super(ctx.getFrame());
		this.context = ctx;
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.buttonLabel = buttonLabel;
        this.fieldLabel = fieldLabel;
        this.parent = rp;
        this.title = title;
		this.builderConsumer = builderConsumer;
		this.onAfterCreated  = onAfterCreated;
        setSize(new Dimension(DLG_WIDTH,DLG_HEIGHT));
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        this.prefs = Preferences.userRoot().node(BLTProperties.PREFERENCES_NAME);
        
        this.existingNames = new ArrayList<>();
        @SuppressWarnings("unchecked")
		Enumeration<AbstractNavTreeNode> walker = parent.children();
        while( walker.hasMoreElements()) {
        	existingNames.add(walker.nextElement().getName());
        }
  
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
	    FileNameExtensionFilter filter = new FileNameExtensionFilter("JSON", "json", "txt");
	    fc.setFileFilter(filter);
	    String startDirectoryName = prefs.get(BLTProperties.PREF_EXIM_DIRECTORY,System.getProperty(BLTProperties.EXIM_PATH));
	    if(startDirectoryName!=null ) {
	    	File startDirectory = new File(startDirectoryName);
		    fc.setCurrentDirectory(startDirectory);
	    }
	    
	    fc.setDialogTitle(title);
	    fc.setApproveButtonText(buttonLabel);
	    fc.setEnabled(false);
	    fc.setMultiSelectionEnabled(false);
	    fc.setName(FILE_CHOOSER_NAME);
	    fc.addActionListener(this);
	    
		columnConstraints = "";
		layoutConstraints = "fillx,ins 10";
		rowConstraints = "";
	    JPanel namePanel = new JPanel(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
	    JLabel label = new JLabel(fieldLabel);
	    this.diagramName = new ValidatedTextField(new JTextField("", 30)) {
			private static final long serialVersionUID = 3930811570476547474L;

			protected String validate(String textValue) {
				if (StringUtils.isBlank(textValue)) {
					return "Name required";
				}
				else if (existingNames.contains(textValue)) {
					return "Name in use";
				}
				return null;
			}
		};
	    namePanel.add(label, ""); 
	    namePanel.add(diagramName,  "pushx, growx"); 
	    add(namePanel, "wrap");
	    add(fc, "wrap");
	}
	
	
	/**
	 * @return the file path that the user selected from the chooser.
	 */
	public File getFilePath() { return filePath; }
	public String getDiagramName() { return diagramName.getField().getText(); }
	
	public void open() {
		pack();
		CommonUI.centerComponent(this, this.context.getFrame());
		setVisible(true);
	}

	public void close() {
		setVisible(false);
		dispose();
	}

	/**
	 * We receive events from the file chooser.
	 * For the text field, the command is the field contents.
	 */
	public void actionPerformed(ActionEvent e) {
		log.infof("%s.actionPerformed %s = %s", CLSS,e.getActionCommand(),((JComponent)(e.getSource())).getName());
		if( e.getActionCommand().equals(JFileChooser.APPROVE_SELECTION)) {
			filePath = fc.getSelectedFile();
			if( filePath!=null ) {
				prefs.put(BLTProperties.PREF_EXIM_DIRECTORY, filePath.getParent());
				if( filePath.exists() && filePath.canRead()) {
					try {
						byte[] bytes = Files.readAllBytes(filePath.toPath());
						// Convert to a serializable diagram 
						ObjectMapper mapper = new ObjectMapper();
						mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
						SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
						if( sd!=null ) {
							log.infof("%s.actionPerformed imported diagram: %s -> %s", CLSS,sd.getName(),getDiagramName());
							sd.setName(getDiagramName());
							sd.setState(DiagramState.DISABLED);

							// Convert back to a resource
							String json = mapper.writeValueAsString(sd);
							DesignableProject project = context.getProject();

							ResourcePath newPath = parent.getChild(new String[] { getDiagramName() });
							ProjectResourceBuilder builder = ProjectResource.newBuilder();

							ImportDialog.this.builderConsumer.accept(builder);

							builder.setProjectName(project.getName())
							.setResourcePath(newPath)
							.putData(json.getBytes());

							ProjectResource newResource = builder.build();
							project.createResource(newResource);
							EventQueue.invokeLater(() -> onAfterCreated.accept(newResource.getResourceId()));
						}
						else {
							ErrorUtil.showWarning(String.format("Failed to deserialize file (%s)",filePath.getAbsolutePath()),POPUP_TITLE);
						}
					}

					catch (ResourceNamingException|RuntimeException ex) {
						ImportDialog.this.close();
						EventQueue.invokeLater(() -> ErrorUtil.showError(ex));
					} 
					catch( FileNotFoundException fnfe) {
						// Should never happen, we just picked this off a chooser
						ErrorUtil.showWarning(String.format("File %s not found",filePath.getAbsolutePath()),POPUP_TITLE); 
					}
					catch( IOException ioe) {
						ErrorUtil.showWarning(String.format("IOException (%s)",ioe.getLocalizedMessage()),POPUP_TITLE); 
					}
					catch(Exception ex) {
						ErrorUtil.showError(String.format("Deserialization exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
					}
				}
				this.dispose();
			}
			else if(e.getActionCommand().equals(JFileChooser.CANCEL_SELECTION)){
				filePath =null;
				this.dispose();
			}
		}
	}
	
	public static ImportDialogBuilder newBuilder() { return new ImportDialogBuilder(); }

	public static class ImportDialogBuilder
	{
		private DesignerContext context;
		private ResourcePath parentPath;
		private Consumer<ProjectResourceBuilder> builderConsumer;
		private AbstractNavTreeNode parent = null;
		private String title = "Import Diagram";
		private String actionText = "Create Resource";
		private String buttonLabel = "Import";
		private String defaultName = "New Resource";
		private String fieldLabel = "Resource Name";
		// When done update the node for "changed"
		private Consumer<ProjectResourceId> onAfterCreated = id -> {
			NodeStatusManager statusManager = NodeStatusManager.getInstance();
			statusManager.setPendingName(id, id.getResourcePath().getName());
			statusManager.getNode(id).select();
		};

		public ImportDialogBuilder setContext(DesignerContext context) {
			this.context = context;
			return this;
		}

		public ImportDialogBuilder setParent(ResourcePath path) {
			this.parentPath = path;
			return this;
		}

		public ImportDialogBuilder setResourceBuilder(Consumer<ProjectResourceBuilder> builderConsumer) {
			this.builderConsumer = builderConsumer;
			return this;
		}

		public ImportDialogBuilder setOnAfterCreated(Consumer<ProjectResourceId> onAfterCreated) {
			this.onAfterCreated = onAfterCreated;
			return this;
		}

		public ImportDialogBuilder setNoun(String noun) {
			this.title = "New " + noun;
			this.actionText = "Import " + noun;
			this.defaultName = "New " + noun;
			this.buttonLabel = BundleUtil.get().getString(PREFIX+".Import.ApproveButton");
			this.fieldLabel = noun + " Name";
			return this;
		}
		
		public ImportDialogBuilder setParent(AbstractNavTreeNode node) {
			this.parent = node;
			return this;
		}
		
		public ImportDialogBuilder setTitle(String title) {
			this.title = title;
			return this;
		}

		public ImportDialogBuilder setDefaultName(String defaultName) {
			this.defaultName = defaultName;
			return this;
		}

		public ImportDialogBuilder setActionText(String actionText) {
			this.actionText = actionText;
			return this;
		}

		public void buildAndDisplay() {
			Preconditions.checkNotNull(this.context);
			Preconditions.checkNotNull(this.parentPath);
			Preconditions.checkNotNull(this.builderConsumer);

			ImportDialog dialog = new ImportDialog(this.context, this.parentPath, this.builderConsumer, this.buttonLabel,this.fieldLabel, this.title, this.parent,this.onAfterCreated);

			dialog.open();
		}
	}
}
