/**
 *   (c) 2012-2022  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.component.beaninfos;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import com.ils.common.component.PrefuseViewerComponent;
import com.inductiveautomation.factorypmi.designer.workspace.WindowWorkspace;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.DesignerContextImpl;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.vision.api.designer.beans.ComponentPopupInitializer;
/**
 * This class controls the custom actions in the popup menu given to the WindowWorkspace. 
 * The inheritance is enforced by the WindowWorkspace.
 * 
 * At this point it does not seem possible to alter any of the standard menu items.
 */
public class BlockComponentInitializer implements ComponentPopupInitializer<PrefuseViewerComponent> {
	private final static String TAG="BlockComponentInitializer";
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	/**
	 * Constructor: The superclass constructor takes an array of relevant custom
	 *               descriptors. The DynamicPropertyProviderCustomizer.VALUE_DESCRIPTOR
	 *               is added here.
	 */
	public BlockComponentInitializer() {
		
	}

	private void positionPopup(JComponent component , JFrame frame, JDialog dlg) {
		Rectangle screen = new Rectangle(frame.getLocationOnScreen(), frame.getSize());

		Rectangle compUnion = component.getBounds();
		int centerX = compUnion.x + compUnion.width / 2;
		int centerY = compUnion.y + compUnion.height / 2;

		Point p = new Point(centerX - dlg.getWidth() / 2, centerY - dlg.getHeight() / 2);

		if (p.y + dlg.getHeight() > screen.y + screen.height)
			p.y = (screen.y + screen.height - dlg.getHeight());
		else if (p.y < screen.y) {
			p.y = screen.y;
		}

		if (p.x + dlg.getWidth() > screen.x + screen.width)
			p.x = (screen.x + screen.width - dlg.getWidth());
		else if (p.x < screen.x) {
			p.x = screen.x;
		}
		dlg.setLocation(p);
	}
	/**
	 * Provide a list of actions to be added to the top of the default set.
	 */
	@Override
	public List<Action> getActions(final List<PrefuseViewerComponent> components, final WindowWorkspace workspace) {
		List<Action> actions = new ArrayList<Action>();

		if(components.size()==0) return actions;

		// Assume that we are operating on only a single object
		final PrefuseViewerComponent block = (PrefuseViewerComponent)components.get(0);
		// They make it really hard to track down the "IgnitionDesigner" object .. but here's how - Note EREIAM JH 
		IgnitionDesigner dsnr = null;
		try {
			DesignerContextImpl contextImplementation = (DesignerContextImpl)workspace.getDesignerContext();
			dsnr = (IgnitionDesigner)contextImplementation.getFrame();
		}
		catch(ClassCastException cce) {
			log.error(TAG+" Cast error ("+cce.getLocalizedMessage()+")");
		}
		final IgnitionDesigner designer = dsnr;

		
		
		// ========================= These are the standard entries. Don't duplicate. ======================
		/*
           final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
           BaseAction cut = new BaseAction("fpmi.Actions.Cut", IconUtil.getIcon("cut")) {
        	   public void actionPerformed(ActionEvent e) {
        		   Transferable t = workspace.getEditActionHandler().doCut();
        		   if (t != null) {
        			   try { clipboard.setContents(t, null);} 
        			   catch (IllegalStateException ex) {
        				   logger.error(TAG+"PopupInitializer: Error putting cut object into clipboard.("+ ex.getLocalizedMessage()+")");
        			   }
        		   }
        	   }  
           };
           actions.add(cut);
           BaseAction copy = new BaseAction("fpmi.Actions.Copy", IconUtil.getIcon("copy")) {
        	   public void actionPerformed(ActionEvent e) {
        		   Transferable t = workspace.getEditActionHandler().doCopy();
        		   if (t != null) {
        			   try { clipboard.setContents(t, null);} 
        			   catch (IllegalStateException ex) {
        				   logger.error(TAG+"PopupInitializer: Error putting copied object into clipboard.("+ ex.getLocalizedMessage()+")");
        			   }
        		   }
        	   }  
           };
          actions.add(copy);
          BaseAction paste = new BaseAction("fpmi.Actions.Paste", IconUtil.getIcon("paste")) {
        	  public void actionPerformed(ActionEvent e) {
        		  Transferable t = clipboard.getContents(null);
        		  if (t != null) {
        			  workspace.getEditActionHandler().doPaste(t); 

        		  }
        	  }  
          };
          actions.add(paste);
          BaseAction delete = new BaseAction("fpmi.Actions.Delete", IconUtil.getIcon("delete")) {
        	  public void actionPerformed(ActionEvent e) {
        		  workspace.getEditActionHandler().doDelete();
        	  }  
          };
          actions.add(delete);

          BaseAction lock = new StateChangeAction("fpmi.Actions.Lock", IconUtil.getIcon("lock")) {
        	  public void actionPerformed(ActionEvent e)
        	  {
        		  if (workspace.getSelectedItemsCount() > 0) {
        			  List<JComponent> components = workspace.getSelectedItems();
        			  for (JComponent comp : components)
        				  comp.putClientProperty("v.lck", isSelected() ? Boolean.TRUE : null);
        		  }
        		  setSelected(!isSelected());
        	  }

        	  public void itemStateChanged(ItemEvent evt) {
        		  boolean b = evt.getStateChange() == 1;
        		  if (b) {
        			  putValue("Name", BundleUtil.get().getString("fpmi.Actions.Lock.Name"));
        			  putValue("SmallIcon", IconUtil.getIcon("lock", IconUtil.S_16));
        		  } else {
        			  putValue("Name", BundleUtil.get().getString("fpmi.Actions.Lock.Name.Unlocked"));
        			  putValue("SmallIcon", IconUtil.getIcon("lock_open", IconUtil.S_16));
        		  }
        	  }
          };
          actions.add(lock);

          BaseAction position = new BaseAction("fpmi.Actions.ConfigurePosition", IconUtil.getIcon("layout_edit")) {
        	  @SuppressWarnings("static-access")
        	  public void actionPerformed(ActionEvent e) {
        		  List<JComponent> items = workspace.getSelectedItems();
        		  PositionDialog dialog = null;
        		  if ((workspace.getSelectedItemsCount() > 0) && 
        				  ((workspace.getLayoutManipulator().componentsAreSiblings(items)) || 
        						  (items.get(0) instanceof TopLevelContainer)))
        		  {
        			  dialog = new PositionDialog(frame);
        		  }

        		  if ((items.get(0) instanceof TopLevelContainer)) {
        			  dialog.setComponents(items, null);
        		  }
        		  else {
        			  dialog.setComponents(items, (VisionContainer)((JComponent)items.get(0)).getParent());
        		  }

        		  positionPopup(block,frame,dialog);
        		  dialog.setVisible(true);
        	  };

          };
          actions.add(position);
		 */
/*
		BaseAction events = new BaseAction(PREFIX+"ActionScript", IconUtil.getIcon("scroll2")) {
			public void actionPerformed(ActionEvent e) {
				ActionConfigDialog actionDialog = new ActionConfigDialog(frame, BundleUtil.get().getStringLenient("fpmi.ConfigureActions.DialogTitle"), workspace.getVisionDesigner());
				Method selectMethod = null;
				try {
					selectMethod = block.getClass().getMethod("onSelect", new Class[] { });
				} 
				catch (SecurityException se) {
					se.printStackTrace();
				} 
				catch (NoSuchMethodException nsm) {
					nsm.printStackTrace();
				}
				actionDialog.showDialog(block, selectMethod);
			}
		};
		actions.add(events);

		BaseAction scriptModule = new BaseAction(PREFIX+"ModelScript", IconUtil.getIcon("scroll")) {
			public void actionPerformed(ActionEvent e) {
				designer.showScriptPlayground();
			}
		};
		actions.add(scriptModule);
		
		*/
		return actions;


	}
}
