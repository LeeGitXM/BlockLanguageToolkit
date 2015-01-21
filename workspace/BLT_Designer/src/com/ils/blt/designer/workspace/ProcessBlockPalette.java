/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.designer.BLTDesignerHook;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.designable.tools.AbstractDesignTool;
import com.inductiveautomation.ignition.designer.gui.DragInitiatorListener;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspaceFrame;
import com.jidesoft.docking.DockableFrame;

/**
 * A block palette is a dockable frame that holds icons that represent executable blocks. 
 * 
 */
public class ProcessBlockPalette extends DockableFrame implements ResourceWorkspaceFrame{
	private static final long serialVersionUID = 4627016359409031941L;
	private static final String TAG = "ProcessBlockPalette";
	public static final String DOCKING_KEY = "ProcessBlockPalette";
	private static final Dimension IMAGE_SIZE = new Dimension(32,32);
	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	
	/**
	 * Constructor 
	 */
	public ProcessBlockPalette(DesignerContext ctx,DiagramWorkspace workspace) {
		super(DOCKING_KEY, IconUtil.getRootIcon("delay_block_16.png"));  // Pinned icon
		setUndockedBounds(new Rectangle(200, 100, 550, 130));
		this.context = ctx;
		setAutohideHeight(100);
		setAutohideWidth(120);
		setDockedHeight(100);
		setDockedWidth(120);
		
		this.workspace = workspace;
		
		// Query the Gateway for a list of blocks to display
		JTabbedPane tabbedPane = new JTabbedPane();
		ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
		List<PalettePrototype> prototypes = handler.getBlockPrototypes();
		for( PalettePrototype proto:prototypes) {
			JComponent component = new PaletteEntry(proto).getComponent();
			JPanel panel = null;
			String tabName = proto.getTabName();
			int tabIndex = tabbedPane.indexOfTab(tabName);
			if( tabIndex < 0 ) {    // Prototype references a new tab
				panel = new JPanel();
				tabbedPane.addTab(tabName, panel);
			}
			else {
				panel = (JPanel)tabbedPane.getComponentAt(tabIndex);
			}
			if(component!=null) panel.add(component);
		}

		setContentPane(tabbedPane);
	}


	@Override
	public String getKey() {
		return DOCKING_KEY;
	}


	@Override
	public boolean isInitiallyVisible() {
		return true;
	}
	

	private class PaletteEntry extends AbstractAction {
		private static final long serialVersionUID = 6689395234849746852L;
		private final PalettePrototype prototype;
		private JPanel panel = null;

		public PaletteEntry(PalettePrototype proto) {
			super(TAG);
			prototype = proto;
			log.debugf("%s: PaletteEntry %s",TAG,proto.getPaletteIconPath());
			Image img = ImageLoader.getInstance().loadImage(proto.getPaletteIconPath(),IMAGE_SIZE);
			ImageIcon icon = null;
			if( img !=null) icon = new ImageIcon(img);
			if( icon!=null ) {
				JToggleButton button = new JToggleButton(icon);
				button.setToolTipText(prototype.getTooltipText());
				button.setBorderPainted(false);
				button.setContentAreaFilled(false);
				button.setMargin(new Insets(1,1,1,1));
				button.addActionListener(this);
				
				// Make a drag enabled
				button.setTransferHandler(new BlockTransferHandler(proto));
				DragInitiatorListener.install(button);
				
				JLabel label = new JLabel(prototype.getPaletteLabel());
				label.setHorizontalAlignment(SwingConstants.CENTER);
				Font font = new Font(label.getFont().getFontName(),Font.PLAIN,label.getFont().getSize()-2);
				label.setFont(font);
				panel = new JPanel(new BorderLayout());
				panel.add(button,BorderLayout.CENTER);
				panel.add(label,BorderLayout.SOUTH);
			}
			else {
				log.warnf("%s: PaletteEntry icon %s not found. Palette entry %s ignored.",TAG,proto.getPaletteIconPath(),prototype.getPaletteLabel());
			}
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if( workspace.getSelectedContainer()!=null ) {
				log.debugf("%s: PaletteEntry creating process view block",TAG);
				ProcessBlockView blk = new ProcessBlockView(prototype.getBlockDescriptor());  
				workspace.setCurrentTool(new InsertBlockTool(blk));
			}
			log.tracef("%s: PaletteEntry action performed complete",TAG);
		}
		public JComponent getComponent() { return panel; }
	}
	
	private class InsertBlockTool extends AbstractDesignTool {
		
		private final ProcessBlockView block;
		public InsertBlockTool(ProcessBlockView blk) {
			block = blk;
		}
		
		@Override
		public Cursor getCursor(Point point, int inputEventMask) {
	
			return Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);
		}
		
		@Override
		public void onPress(Point p, int modifiers) {
			BlockDesignableContainer c = (BlockDesignableContainer)findDropContainer(p);
			BlockDiagramModel model = c.getModel();
			block.setLocation(p);
			model.addBlock(block);
			workspace.setCurrentTool(workspace.getSelectionTool());
		}
	}
}
