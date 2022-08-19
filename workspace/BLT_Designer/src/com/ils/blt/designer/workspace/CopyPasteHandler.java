/**
 *   (c) 2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.EventQueue;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Optional;
import java.util.function.Consumer;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.ils.blt.designer.navtree.NewResourceDialog;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.project.DesignableProject;

/**
 * Handle the nitty-gitties of Copy/Paste as requested from the Nav tree. There are two "flavors".
 * From a DiagramTreeNode, a diagram can be copied. From a NavTreeFolderNode, a tree of folders and diagrams
 * can be copied. Paste is only available on a folder node. The paste is whatever is in the buffer. 
 * A node cannot paste on itself.
 */

public class CopyPasteHandler  {
	private static final String CLSS = "CopyPasteHandler";
	public static final String ARCHIVE_TYPE_DIAGRAM = "blt.diagram";
	public static final String ARCHIVE_TYPE_FOLDER = "blt.folder";
	public static final String ENTRY_DELIMITER = "|";
	public static final String KEY_DELIMITER = ":";
	private final DesignerContext context;
	private final AbstractResourceNavTreeNode parent;
	private final static LoggerEx log = LogUtil.getLogger(NavTreeFolder.class.getPackageName());
	private final Consumer<ProjectResourceId> onAfterCreated;

	/**
	 * Constructor: Provide the node for which this applies
	 */
	public CopyPasteHandler(DesignerContext ctx,AbstractResourceNavTreeNode node) {
		this.context = ctx;
		this.parent = node;
		this.onAfterCreated = id -> {
			NodeStatusManager statusManager = NodeStatusManager.getInstance();
			statusManager.setPendingName(id, id.getResourcePath().getName());
			statusManager.getNode(id).select();
		};
	}

	/**
	 * @return true if the clipboard contains something appropriate
	 */
	public boolean canPaste() {
		boolean result = false;
		final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

		Transferable t = clipboard.getContents(null);
		if (t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
			try { 
				String clipData = (String)t.getTransferData(DataFlavor.stringFlavor);
				if( clipData.startsWith(ARCHIVE_TYPE_DIAGRAM) ||
					clipData.startsWith(ARCHIVE_TYPE_FOLDER)  ) {
					result = true;
				}
			} 
			catch (Exception ex) {
				result = false;
				log.errorf("%s.canPaste: Unhandled Exception i(%s)",CLSS,ex.getMessage());
				ex.printStackTrace();
			}
		}
		return result;
	}
	
	/**
	 * The transferable data consists of a key meaning diagram, a delimiter, then the resource path.
	 */
	public void doCopyDiagram() {
		final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		String data = parent.getResourcePath().getPath().toString();
		Transferable t =  new StringSelection(String.format("%s%s%s",ARCHIVE_TYPE_DIAGRAM,KEY_DELIMITER,data));

		if (t != null) {
			try { 
				clipboard.setContents(t, null); 
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("doCopyDiagram: Unhandled Exception (%s)",ex.getMessage()), "Copy Diagram");
			}
		}
	}

	/**
	 * The transferable data consists of a key meaning folder, a delimiter, then the resource path.
	 * This is followed by similar entries for descendants (folders and diagrams) separated by a "|"
	 * delimiter. Other than the root, order is not important.
	 */
	public void doCopyFolder() {
		final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		String data = parent.getResourcePath().getPath().toString();
		StringBuilder builder = new StringBuilder();
		builder.append(String.format("%s%s%s",ARCHIVE_TYPE_FOLDER,KEY_DELIMITER,data));  // Root node
		appendDescendantPaths(parent,builder);
		Transferable t =  new StringSelection(builder.toString());

		if (t != null) {
			try { 
				clipboard.setContents(t, null); 
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("doCopyFolder: Unhandled Exception (%s)",ex.getMessage()), "Copy Folder Tree");
			}
		}
	}
	
	public void doPaste() {
		final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

		Transferable t = clipboard.getContents(null);
		if (t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
			try { 
				String clipData = (String)t.getTransferData(DataFlavor.stringFlavor);
				if( clipData.startsWith(ARCHIVE_TYPE_DIAGRAM)) {
					pasteDiagram(StringPath.parse(clipData.substring(ARCHIVE_TYPE_DIAGRAM.length()+KEY_DELIMITER.length())));
				}
				else if(clipData.startsWith(ARCHIVE_TYPE_FOLDER)  ) {
					pasteFolder(clipData);
				}
				else {
					log.warnf("%s.doPaste: No appropriate data found in clipboard",CLSS);
				}
			} 
			catch (Exception ex) {
				log.errorf("%s.doPaste: Unhandled Exception i(%s)",CLSS,ex.getMessage());
				ex.printStackTrace();
			}
		}
	}
	private void appendDescendantPaths(AbstractResourceNavTreeNode node,StringBuilder builder) {
		int childCount = node.getChildCount();
		for( int i=0;i<childCount;i++) {
			AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)node.getChildAt(i);
			String data = child.getResourcePath().getPath().toString();
			String type = ARCHIVE_TYPE_FOLDER;
			if( child instanceof DiagramTreeNode ) type = ARCHIVE_TYPE_DIAGRAM;
			builder.append(String.format("%s%s%s",type,KEY_DELIMITER,data));
			appendDescendantPaths(child,builder);
		}
	}
	
	public ProjectResourceId createResourceId(String path) {
		String projectName = context.getProjectName();
		ProjectResourceId resourceId = new ProjectResourceId(projectName,BLTProperties.DIAGRAM_RESOURCE_TYPE,path);
		return resourceId;
	}
	
	/**
	 * Check children of the parent node to make sure there is no duplication.
	 *  
	 * @param text
	 * @return a vetted name
	 */
	private String ensureUniqueName(String text) {
		String name = text;
		int index = 0;
		while( isNameDuplicated(name)) {
			index++;
			name = String.format("%s-%d", text,index);
		}
		return name;
	}
	
	/**
	 * @param name to be tested
	 * @return true if the supplied name matches any of the child node names.
	 */
	private boolean isNameDuplicated(String name) {
		boolean result = false;
		@SuppressWarnings("unchecked")
		Enumeration<AbstractResourceNavTreeNode> e = parent.children();
		while(e.hasMoreElements()) {
			AbstractResourceNavTreeNode node = e.nextElement();
			if(node.getName().equalsIgnoreCase(name)) {
				result = true;
				break;
			}
		}
		return result;
	}
	
	/**
	 * Add a cloned diagram to the current node.
	 * @param stringPath path to the original copied diagram
	 */
	private void pasteDiagram(StringPath stringPath) {
		// Guarantee that the root node name does not conflict with any of the current node's children
		String name = stringPath.getLastPathComponent();
		name = ensureUniqueName(name);
		ProjectResourceId source = createResourceId(stringPath.toString());
		StringPath destinationPath = StringPath.extend(parent.getResourcePath().getPath(),name);
		ProjectResourceId destination = createResourceId(destinationPath.toString());
		createProjectResource(ARCHIVE_TYPE_DIAGRAM,source,destination);
		EventQueue.invokeLater(() -> onAfterCreated.accept(destination));
	}
	/**
	 * Add a folder and descendant folders and diagrams to the current node.
	 * @param stringEntries clipboard data
	 */
	private void pasteFolder(String stringEntries) {
		// Guarantee that the root node name does not conflict with any of the current node's children
		// The root node is the only one to worry about name conflicts.
		String[] entries = stringEntries.split(ENTRY_DELIMITER);
		String root = entries[0].split(KEY_DELIMITER)[1];
		String name = StringPath.parse(root).getLastPathComponent();
		name = ensureUniqueName(name);
		ProjectResourceId source = createResourceId(root);
		StringPath destinationPath = StringPath.extend(parent.getResourcePath().getPath(),name);
		ProjectResourceId destination = createResourceId(destinationPath.toString());
		createProjectResource(ARCHIVE_TYPE_FOLDER,source,destination);
		EventQueue.invokeLater(() -> onAfterCreated.accept(destination));
		
		// Now create all the child resources
		boolean hasRoot = false;
		for(String entry:entries) {
			if(!hasRoot) {
				hasRoot = true;
				continue;  // We've already handled the root node
			}
			String[] keyPath = entry.split(KEY_DELIMITER);
			String key = keyPath[0];
			String path= keyPath[1];
			source = createResourceId(path);
			path = path.substring(root.length()+1);  // Now partial path w/ respect to root
			destinationPath = StringPath.extend(parent.getResourcePath().getPath(),path);
			ProjectResourceId child = createResourceId(destinationPath.toString());
			createProjectResource(key,source,child);
			EventQueue.invokeLater(() -> onAfterCreated.accept(child));
		}
	}
	
	/**
	 * Copy the source resource into a project resource with the specified new resourceId.
	 * The project will become saved after a normal save action.
	 */
	private void createProjectResource(String key,ProjectResourceId source,ProjectResourceId destination) {
		ProjectResourceBuilder builder = ProjectResource.newBuilder();
		DesignableProject project = context.getProject();
		
		if( key.equalsIgnoreCase(ARCHIVE_TYPE_DIAGRAM)) {
			ProcessDiagramView view = null;
			
			// The diagram may be held by the status manager, if dirty
			NodeStatusManager statusManager = NodeStatusManager.getInstance();
			if( statusManager.getPendingView(source)!=null ) {
				view = statusManager.getPendingView(source).clone();  // Changes block Ids
			}
			else {
				Optional<ProjectResource> optional = project.getResource(source);
				ProjectResource pr = optional.get();
				ObjectMapper mapper = new ObjectMapper();
				byte[] bytes = pr.getData();
				if( key.equals(ARCHIVE_TYPE_FOLDER )|| bytes.length>0 ) {
					try {
						SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
						if( sd!=null ) {
							view = new ProcessDiagramView(context,source,sd);
						}
					}
					catch(JsonParseException jpe) {
						log.warnf("%s.createProjectResource: Parse exception saving %s...(%s)", CLSS, pr.getResourceName(),jpe.getLocalizedMessage());
						return;
					}
					catch(JsonMappingException jme) {
						log.warnf("%s.createProjectResource: Mapping exception saving %s...(%s)", CLSS, pr.getResourceName(),jme.getLocalizedMessage());
						return;
					}
					catch(IOException ioe) {
						log.warnf("%s.createProjectResource: IO exception saving %s...(%s)", CLSS, pr.getResourceName(),ioe.getLocalizedMessage());
						return;
					}
				}
				else {
					log.warnf("%s.createProjectResource: Diagram %s has no data", CLSS, source.getFolderPath());
					return;
				}
			}
			builder.setApplicationScope(ApplicationScope.GATEWAY);
			builder.setFolder(false);
			SerializableDiagram sd = view.clone().createSerializableRepresentation();
			sd.setPath(destination.getFolderPath());
			sd.setName(destination.getResourcePath().getName());
			builder.putData(sd.serialize());

		}
		else {     // Folder
			builder.setApplicationScope(ApplicationScope.GATEWAY);
			builder.setFolder(true);
		}
		builder.setProjectName(project.getName());
		builder.setResourcePath(destination.getResourcePath());
		ProjectResource res = builder.build();
		project.createOrModify(res);
	}
}
