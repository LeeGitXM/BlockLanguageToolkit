/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.client;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.UIManager;

import com.ils.jgx.editor.JgxPalette;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.mxgraph.swing.util.mxSwingConstants;
import com.mxgraph.util.mxConstants;

/**
 *  This class exposes functions to define diagrams from the client 
 *  - and to report completion execution.
 *  
 *  Remote procedure calls are made to the Gateway scope to produce the changes.
 */
public class GraphicsScriptFunctions  {
	private static final String TAG = "DiagnosticsScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(GraphicsScriptFunctions.class.getPackage().getName());

	/**
	 * Define a frame for later retrieval.
	 * 
	 * @param key the client session ID
	 * @param pyFrame a JFrame as perceived in the Jython world.
	 */
	public static void setFrame(String key, JFrame pyFrame)  {
		FrameRepository.getInstance().setFrame(key, pyFrame);
	}
	
	/**
	 * Find a previously defined frame.
	 * 
	 * @param key the client session ID
	 * @return a JFrame as perceived in the Jython world.
	 */
	public static JFrame getFrame(String key)  {
		return FrameRepository.getInstance().getFrame(key);
	}

	/**
	 * Display the widget palette in the specified frame.
	 * 
	 * @param frm a JFrame in which to display the palette
	 */
	public static void displayPalette(JFrame frm)  {
		log.info(String.format("%s.displayPalette: %s", TAG,frm.getClass().getCanonicalName()));
		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}

		mxSwingConstants.SHADOW_COLOR = Color.LIGHT_GRAY;
		mxConstants.W3C_SHADOWCOLOR = "#D3D3D3";
		JFrame frame = new JFrame("Block Palette");
/*
		JgxPalette editor = new JgxPalette();
		
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		editor.setPreferredSize(new Dimension(870,640));
		frame.getContentPane().add(editor,BorderLayout.CENTER);
		*/
		frame.pack();
		frame.setVisible(true);
	}

	

}