/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.TransferHandler;

import com.ils.block.common.PalettePrototype;
import com.inductiveautomation.ignition.client.util.LocalObjectTransferable;
import com.inductiveautomation.ignition.common.config.ObservablePropertySet;

public class BlockTransferHandler extends TransferHandler {
	private static final long serialVersionUID = 442737208427664520L;
	PalettePrototype prototype;

	public BlockTransferHandler(PalettePrototype prototype) {
		this.prototype = prototype;
	}

	@Override
	public boolean canImport(JComponent comp, DataFlavor[] transferFlavors) {
		return false;
	}

	@Override
	protected Transferable createTransferable(JComponent c) {
		ProcessBlockView blk = new ProcessBlockView(prototype.getBlockDescriptor());
		return new LocalObjectTransferable(blk, ObservablePropertySet.class);
	}

	@Override
	protected void exportDone(JComponent source, Transferable data, int action) {
		/*
		 * ICK - this is a hack. Swing doesn't deliver a mouse_exited event on a drop, which means the buttons look like
		 * they're "stuck down"
		 */
		MouseListener[] listeners = source.getMouseListeners();
		MouseEvent me = new MouseEvent(source, MouseEvent.MOUSE_EXITED, System.currentTimeMillis(), 0, 0, 0, 0, false);
		for (int i = listeners.length - 1; i >= 0; i--) {
			listeners[i].mouseExited(me);
		}

		((AbstractButton) source).setSelected(false);
	}

	@Override
	public int getSourceActions(JComponent c) {
		return COPY;
	}

	@Override
	public boolean importData(JComponent comp, Transferable t) {
		return false;
	}
}
