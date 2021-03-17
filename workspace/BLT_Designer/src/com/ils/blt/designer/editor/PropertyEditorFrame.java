/**
 *   (c) 2013-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.designable.DesignableWorkspaceAdapter;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.jidesoft.docking.DockableFrame;

/**
 * A PropertyEditorFrame is a DockableFrame in the JIDE workspace (lower left) meant
 * to hold an editor for properties of the selected block, connection or NavTree node. 
 */
@SuppressWarnings("serial")
public class PropertyEditorFrame extends DockableFrame implements ResourceWorkspaceFrame{
	private static final String CLSS = "PropertyEditorFrame";
	public static final String DOCKING_KEY = "ProcessDiagramEditorFrame";
	public static final String TITLE = "Symbolic AI Property Editor";
	public static final String SHORT_TITLE = "Properties";
	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	private final JPanel contentPanel;
	private AbstractPropertyEditor editor = null;
	private ILSLogger log = LogMaker.getLogger(getClass().getPackage().getName());
	
	/**
	 * Constructor 
	 */
	public PropertyEditorFrame(DesignerContext ctx,DiagramWorkspace workspace) {
		super(DOCKING_KEY, IconUtil.getRootIcon("delay_block_16.png"));  // Pinned icon
		this.context = ctx;
		log.infof("%s.PropertyEditorFrame: CONSTRUCTOR ...",CLSS);
		this.workspace = workspace;
		workspace.addDesignableWorkspaceListener(new DiagramWorkspaceListener());
		contentPanel = new JPanel(new BorderLayout());
		init();	
	}


	@Override
	public String getKey() {
		return DOCKING_KEY;
	}

	/** 
	 * Initialize the UI components. The "master" version of the block's
	 * properties resides in the gateway.
	 */
	private void init() {
		setTitle(TITLE);
		setTabTitle(SHORT_TITLE);
		setSideTitle(SHORT_TITLE);
		
		setContentPane(contentPanel);
		contentPanel.setBorder(BorderFactory.createEtchedBorder());
	}
	

	@Override
	public boolean isInitiallyVisible() {
		return true;
	}
	
	public AbstractPropertyEditor getEditor() {
		return editor;
	}

	// A selection was made in the NavTree that we want to edit
	public void setEditor(AbstractPropertyEditor eddy) {
		if( editor!=null ) editor.shutdown();
		this.editor = eddy;
	}
	
	public void refreshPropertyEditor() {
		log.infof("%s.refreshPropertyEditor:",CLSS);
		if( editor!=null) {
			editor.shutdown();
			if( editor instanceof BlockPropertyEditor) {
				editor =  new BlockPropertyEditor(context,workspace,((BlockPropertyEditor)editor).getBlock());
			}
			else if( editor instanceof ApplicationPropertyEditor) {
				editor =  new ApplicationPropertyEditor(context,((ApplicationPropertyEditor)editor).getApplication(),editor.getResource());
			}
			else if(editor instanceof FamilyPropertyEditor) {
				editor =  new FamilyPropertyEditor(context,((FamilyPropertyEditor)editor).getFamily(),editor.getResource());
			}
		}

		//Create a scroll pane
	    JScrollPane scrollPane = new JScrollPane(editor);
		contentPanel.add(scrollPane,BorderLayout.CENTER);
		validate();
	}
	

	private class DiagramWorkspaceListener extends DesignableWorkspaceAdapter {
		// Triggered by DiagramWorkspace.fireSelectedItemsChanged
		@Override
		public void itemSelectionChanged(List<JComponent> selections) {
			if( selections!=null && selections.size()==1 ) {
				JComponent selection = selections.get(0);
				log.infof("%s: DiagramWorkspaceListener.itemSelectionChanged: selected a %s",CLSS,selection.getClass().getName());
				// KLUDGE ALERT: There should be a way to not hard code this.
				if( selection instanceof BlockComponent ) {
					if( editor!=null ) editor.shutdown();
					BlockComponent bc = ( BlockComponent)selection;
					ProcessBlockView blk = (ProcessBlockView)bc.getBlock();
					if( blk.getClassName().contains("FinalDiagnosis")) {
						editor = new FinalDiagnosisPropertyEditor(context,workspace,blk);
					}
					else {
						editor = new BlockPropertyEditor(context,workspace,blk);
					}
					contentPanel.removeAll();
					//Create a scroll pane
				    JScrollPane scrollPane = new JScrollPane(editor);
					contentPanel.add(scrollPane,BorderLayout.CENTER);
					validate();
					return;
				}
				// There may be a connection selected
				else {
					BlockDesignableContainer container = ( BlockDesignableContainer)selection;
					Connection cxn = container.getSelectedConnection();
					if( cxn!=null && cxn.getOrigin()!=null && cxn.getTerminus()!= null ) {
						// The block is a ProcessBlockView.
						// The origin is a BasicAnchorPoint (extends AnchorPoint).
						log.debugf("%s: DiagramWorkspaceListener: connection origin is a %s",CLSS,cxn.getOrigin().getClass().getName());
						log.debugf("%s: DiagramWorkspaceListener: connection id is a %s",CLSS,cxn.getOrigin().getId().getClass().getName());
						log.debugf("%s: DiagramWorkspaceListener: connection block is a %s",CLSS,cxn.getOrigin().getBlock().getClass().getName());
						ConnectionPropertyEditor editr = new ConnectionPropertyEditor(context,cxn);
						contentPanel.removeAll();
						//Create a scroll pane
					    JScrollPane scrollPane = new JScrollPane(editr);
						contentPanel.add(scrollPane,BorderLayout.CENTER);
						return;
					}
				}
			}
		}
		@Override 
		public void containerClosed(DesignableContainer container) {
			log.infof("%s: DiagramWorkspaceListener.containerClosed",CLSS);
		}
		@Override 
		public void containerOpened(DesignableContainer container) {
			log.infof("%s: DiagramWorkspaceListener.containerOpened",CLSS);
		}
	}
	
}
