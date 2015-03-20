package com.ils.blt.designer.workspace;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import java.util.Iterator;

public class DualListTest {

	private static void createAndShowGUI() {
		JFrame frame = new JFrame("Dual List Box Tester");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		DualListBox dual = new DualListBox();
		
		dual.addSourceElements(new String[] {"One", "Two", "Three"});
		
		frame.add(dual, BorderLayout.CENTER);
		frame.setSize(400,300);
		frame.setVisible(true);
	}

	public static void main(String[] args) {
		//Schedule a job for the event-dispatching thread:
        //creating and showing this application's GUI.
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
	}
}
