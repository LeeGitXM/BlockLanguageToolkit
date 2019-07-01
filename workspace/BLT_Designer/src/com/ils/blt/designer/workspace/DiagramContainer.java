package com.ils.blt.designer.workspace;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockProperty;
import com.inductiveautomation.ignition.designer.blockandconnector.AbstractBlockWorkspace;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.ConnectionPainter;
import com.inductiveautomation.ignition.designer.blockandconnector.routing.EdgeRouter;

public class DiagramContainer extends BlockDesignableContainer {
	private static final long serialVersionUID = 7484274138362308991L;
	private static final double WATERMARK_ROTATION = -0.3; // Radians counter-clockwise
	private final UtilityFunctions fncs;

	public DiagramContainer(AbstractBlockWorkspace workspace,BlockDiagramModel model,EdgeRouter router,ConnectionPainter painter) {
		super(workspace, model, router, painter);
		fncs = new UtilityFunctions();
		
	}
	
	
	
	
	
	//  EREIAM JH - Can we override drag and drop functions here?  Bock multiple input connections for a single input block?
	
	
	
	public void ssadfetName (String newName) {
	
	}
	
	
	
	/**
	 * In addition to immediately setting the tab name, we set in the model in case the
	 * component gets re-painted.
	 * @param newName
	 */
	@Override
	public void setName (String newName) {
		((ProcessDiagramView)getModel()).setDiagramName(newName);
		super.setName(newName);	
	}
	
	@Override
	protected void paintComponent(Graphics _g) {
		super.paintComponent(_g);
		Graphics2D g = (Graphics2D) _g;
		
		// First, paint a watermark, if it exists
		ProcessDiagramView diagram = (ProcessDiagramView)getModel();
		String watermark = diagram.getWatermark();
		if( watermark!=null && !watermark.isEmpty() ) {
			float x = (float) (diagram.getDiagramSize().getWidth()/2);
			float y = (float) (diagram.getDiagramSize().getHeight()/2);
			int size = 16;
			if(watermark.length()<5) size = 240;
			else if(watermark.length()<9) size = 124;
			else if(watermark.length()<17) size = 96;
			else if(watermark.length()<33) size = 48;
			AffineTransform save = g.getTransform();
			g.translate(x, y);
			g.rotate(WATERMARK_ROTATION);
			paintTextAt(g,watermark,0,0, Color.LIGHT_GRAY,size); 
			g.setTransform(save);
		}
		// Paint "displayed" properties.
		for(Block blk:getModel().getBlocks() ) {
			ProcessBlockView pbv = (ProcessBlockView)blk;
			float xpos = pbv.getLocation().x;
			float ypos = pbv.getLocation().y;
			if(pbv.isNameDisplayed() ) {
				paintTextAt(g,pbv.getName(),xpos+pbv.getNameOffsetX(),ypos+pbv.getNameOffsetY(),Color.DARK_GRAY,12);
			}
			
			for(BlockProperty bp:pbv.getProperties()) {
				if(bp.isDisplayed() && bp.getValue()!=null) {
					String val = fncs.coerceToString(bp.getValue());
					paintTextAt(g,val,xpos+bp.getDisplayOffsetX(),ypos+bp.getDisplayOffsetY(),Color.DARK_GRAY,12);
				}
			}
		}
	}
	
	/**
	 * Utility method to paint a text string.
	 * @param g
	 * @param text
	 * @param xpos center of the text
	 * @param ypos center of the text
	 * @param fill color of the text
	 */
	private void paintTextAt(Graphics2D g, String text, float xpos, float ypos, Color fill,int fontSize) {
		Font font = g.getFont();
		font = font.deriveFont((float)fontSize);

		FontRenderContext frc = g.getFontRenderContext();
		GlyphVector vector = font.createGlyphVector(frc, text);
		Rectangle2D bounds = vector.getVisualBounds();
		// xpos, ypos are centers. Adjust to upper left.
		ypos+= (float)(bounds.getHeight()/2);
		xpos-= (float)(bounds.getWidth()/2);
		
		/*
		System.out.println(String.format("DiagramContainer: %s at %3.0f,%3.0f size %3.0f,%3.0f in %3.0f,%3.0f",text,xpos,ypos,
				bounds.getWidth(),bounds.getHeight(),
				((ProcessDiagramView)getModel()).getDiagramSize().getWidth(),((ProcessDiagramView)getModel()).getDiagramSize().getHeight()));
		*/
		Shape textShape = vector.getOutline(xpos, ypos);
		g.setColor(fill);
		g.fill(textShape);
	}
}
