package com.ils.blt.designer.navtree;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.MatteBorder;

import org.apache.commons.lang3.StringUtils;

import com.google.common.base.Preconditions;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.IgnitionLookAndFeel;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.client.util.gui.HeaderLabel;
import com.inductiveautomation.ignition.client.util.gui.IgnitionSwingUtilities;
import com.inductiveautomation.ignition.client.util.gui.Later;
import com.inductiveautomation.ignition.client.util.gui.ValidatedTextField;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourceNamingException;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.ResourceUtil;
import com.inductiveautomation.ignition.designer.gui.CommonUI;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.DesignableProject;

import net.miginfocom.swing.MigLayout;

public class NewResourceDialog extends JDialog {
	private static final long serialVersionUID = 5360156442933696991L;
	private final DesignerContext context;
	private final ValidatedTextField name;
	private final CreateAction createAction;
	private final ResourcePath folder;
	private final Consumer<ProjectResourceBuilder> builderConsumer;
	private final Consumer<ProjectResource> onAfterCreated;

	public NewResourceDialog(final DesignerContext ctx,final ResourcePath folder, Consumer<ProjectResourceBuilder> builderConsumer, String title, String defaultName, String actionText, 
			final Predicate<String> namePredicate, Consumer<ProjectResource> onAfterCreated, JComponent extraComponent) {
		super(ctx.getFrame(), title);
		setDefaultCloseOperation(2);

		this.context = ctx;
		this.folder = folder;
		this.builderConsumer = builderConsumer;
		this.onAfterCreated = onAfterCreated;
		this.setName("New Resource");

		this.createAction = new CreateAction(actionText);

		this.name = new ValidatedTextField(new JTextField(defaultName, 30)) {
			private static final long serialVersionUID = 3830811570476547474L;

			protected String validate(String textValue) {
				if (StringUtils.isBlank(textValue)) {
					return "Name required";
				}
				else if (((List<ProjectResource>)context.getProject()
						.browse(folder)
						.orElse(Collections.emptyList()))
						.stream()
						.anyMatch(r -> textValue.equalsIgnoreCase(r.getResourceName()))) {
					return "Name in use";
				}
				else if (!namePredicate.test(textValue)) {
					return "Invalid name";
				}
				return null;
			}
		};

		JPanel bottom = new JPanel(new MigLayout("ins 8px, ax 100%"));
		bottom.setBorder(new MatteBorder(1, 0, 0, 0, IgnitionLookAndFeel.Colors.Base500));
		JButton cancelButton = new JButton(new BaseAction("Actions.Cancel") {
			public void actionPerformed(ActionEvent e) {
				NewResourceDialog.this.close();
			}
		});
		bottom.add(cancelButton);
		JButton okButton = new JButton(this.createAction);
		bottom.add(okButton);

		JPanel pane = new JPanel(new MigLayout("ins 16px"));
		pane.add(new HeaderLabel(BundleUtil.i18n("words.name", new Object[0])), "wrap r");
		pane.add(this.name, "pushx, growx");
		if (extraComponent != null) {
			pane.add(extraComponent, "newline, push, grow");
		}

		JPanel main = new JPanel(new BorderLayout());
		main.add(pane, "Center");
		main.add(bottom, "South");

		setContentPane(main);
		getRootPane().setDefaultButton(okButton);

		this.name.addPropertyChangeListener("dataValid", e -> updateEnabled());
		Later.selectAll(this.name.getField());
		EventQueue.invokeLater(this::updateEnabled);
		IgnitionSwingUtilities.installEscapeCloseAction(this);
	}


	private void updateEnabled() { 
		this.createAction.setEnabled(this.name.isDataValid()); 
	}
	
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
	 * This is the action tied to the OK button on the dialog
	 */
	public class CreateAction extends AbstractAction {
		private static final long serialVersionUID = -3624379559496800384L;

		CreateAction(String text) {
			super(text);
			setEnabled(false);
		}

		@Override
		public void actionPerformed(ActionEvent evt) {
			try {
				DesignableProject project = context.getProject();

				ResourcePath newPath = NewResourceDialog.this.folder.getChild(new String[] { NewResourceDialog.this.name.getField().getText() });
				ProjectResourceBuilder builder = ProjectResource.newBuilder();

				NewResourceDialog.this.builderConsumer.accept(builder);

				builder.setProjectName(project.getName())
				.setResourcePath(newPath);
				

				ProjectResource newResource = builder.build();
				// If diagram, then reset name and path in embedded object
				if( !newResource.isFolder() ) {
					SerializableDiagram sd = SerializableDiagram.deserializeDiagram(newResource);
					sd.setName(newPath.getName());
					sd.setPath(newPath.getFolderPath());
					builder.putData(sd.serialize());
					newResource = builder.build();
				}
				final ProjectResource newres = newResource;
				project.createResource(newResource);
				EventQueue.invokeLater(() -> NewResourceDialog.this.onAfterCreated.accept(newres));
				NewResourceDialog.this.close();
			} 
			catch (ResourceNamingException|RuntimeException ex) {
				NewResourceDialog.this.close();
				EventQueue.invokeLater(() -> ErrorUtil.showError(ex));
			} 
		}
	}

	public static NewResourceDialogBuilder newBuilder() { return new NewResourceDialogBuilder(); }

	public static class NewResourceDialogBuilder
	{
		private DesignerContext context;
		private ResourcePath parentPath;
		private Consumer<ProjectResourceBuilder> builderConsumer;
		private String title = "New Resource";
		private String actionText = "Create Resource";
		private String defaultName = "New Resource";
		private Predicate<String> namePredicate = ResourceUtil::isLegalName;
		// When done update the node for "changed"
		private Consumer<ProjectResource> onAfterCreated = res -> {
			NodeStatusManager statusManager = NodeStatusManager.getInstance();
			ProjectResourceId id = res.getResourceId();
			statusManager.setPendingName(id, id.getResourcePath().getName());
			if( !res.isFolder() ) {
				SerializableDiagram sd = SerializableDiagram.deserializeDiagram(res);
				ProcessDiagramView view = new ProcessDiagramView(context,id,sd);
				statusManager.setPendingView(id, view);
				statusManager.setPendingState(id, sd.getState());
			}
			statusManager.getNode(id).select();
		};
		JComponent extraComponent = null;

		public NewResourceDialogBuilder setContext(DesignerContext context) {
			this.context = context;
			return this;
		}

		public NewResourceDialogBuilder setParent(ResourcePath path) {
			this.parentPath = path;
			return this;
		}

		public NewResourceDialogBuilder setResourceBuilder(Consumer<ProjectResourceBuilder> builderConsumer) {
			this.builderConsumer = builderConsumer;
			return this;
		}

		public NewResourceDialogBuilder setOnAfterCreated(Consumer<ProjectResource> onAfterCreated) {
			this.onAfterCreated = onAfterCreated;
			return this;
		}

		public NewResourceDialogBuilder setNoun(String noun) {
			this.title = "New " + noun;
			this.actionText = "Create " + noun;
			this.defaultName = "New " + noun;
			return this;
		}

		public NewResourceDialogBuilder setTitle(String title) {
			this.title = title;
			return this;
		}

		public NewResourceDialogBuilder setDefaultName(String defaultName) {
			this.defaultName = defaultName;
			return this;
		}

		public NewResourceDialogBuilder setActionText(String actionText) {
			this.actionText = actionText;
			return this;
		}

		public NewResourceDialogBuilder setNamePredicate(Predicate<String> namePredicate) {
			this.namePredicate = namePredicate;
			return this;
		}

		public NewResourceDialogBuilder setExtraComponent(JComponent extraComponent) {
			this.extraComponent = extraComponent;
			return this;
		}

		public void buildAndDisplay() {
			Preconditions.checkNotNull(this.context);
			Preconditions.checkNotNull(this.parentPath);
			Preconditions.checkNotNull(this.builderConsumer);

			NewResourceDialog dialog = new NewResourceDialog(this.context, this.parentPath, this.builderConsumer, this.title, this.defaultName, this.actionText, this.namePredicate, this.onAfterCreated, this.extraComponent);

			dialog.open();
		}
	}
}
