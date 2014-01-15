/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;

import com.ils.block.common.PalettePrototype;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.PropertiesRequestHandler;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.designable.tools.AbstractDesignTool;
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
		JPanel panel = new JPanel();
		PropertiesRequestHandler handler = ((BLTDesignerHook)ctx.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
		List<PalettePrototype> prototypes = handler.getBlockPrototypes();
		for( PalettePrototype proto:prototypes) {
			JComponent component = new PaletteEntry(proto).getComponent();
			if( component!=null)panel.add(component);
		}

		setContentPane(panel);
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
			log.infof("%s: PalleteEntry %s",TAG,proto.getPaletteIconPath());
			Icon icon = IconUtil.getRootIcon(PalettePrototype.class, proto.getPaletteIconPath());
			if( icon!=null ) {
				JToggleButton button = new JToggleButton(icon);
				button.setToolTipText(prototype.getTooltipText());
				button.setBorderPainted(false);
				button.setContentAreaFilled(false);
				button.setMargin(new Insets(1,1,1,1));
				button.addActionListener(this);
				
				JLabel label = new JLabel(prototype.getPaletteLabel());
				label.setHorizontalAlignment(SwingConstants.CENTER);
				Font font = new Font(label.getFont().getFontName(),Font.PLAIN,label.getFont().getSize()-2);
				label.setFont(font);
				panel = new JPanel(new BorderLayout());
				panel.add(button,BorderLayout.CENTER);
				panel.add(label,BorderLayout.SOUTH);
			}
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			log.infof("%s: PalleteEntry action performed",TAG);
			if( workspace.getSelectedContainer()!=null ) {
				log.infof("%s: PalleteEntry creating process view block",TAG);
				ProcessBlockView blk = new ProcessBlockView(prototype.getBlockDescription());  
				workspace.setCurrentTool(new InsertBlockTool(blk));
			}
			log.infof("%s: PalleteEntry action performed complete",TAG);
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
