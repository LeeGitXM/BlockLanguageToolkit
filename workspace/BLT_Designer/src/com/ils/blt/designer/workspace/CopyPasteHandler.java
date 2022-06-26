/**
 *   (c) 2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;

import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;

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
	private final AbstractResourceNavTreeNode parent;
	private final static LoggerEx log = LogUtil.getLogger(NavTreeFolder.class.getPackageName());

	/**
	 * Constructor: Provide the node for which this applies
	 */
	public CopyPasteHandler(AbstractResourceNavTreeNode node) {
		this.parent = node;
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
					pasteDiagram(clipData.substring(ARCHIVE_TYPE_DIAGRAM.length()+KEY_DELIMITER.length()));
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
	
	private void pasteDiagram(String stringPath) {
		// Guarantee that the root node name does not conflict with any of the current node's children
	}
	
	private void pasteFolder(String stringEntries) {
		// Guarantee that the root node name does not conflict with any of the current node's children
		String[] entries = stringEntries.split(ENTRY_DELIMITER);
		String root = entries[0].split(KEY_DELIMITER)[1];
	}
}
