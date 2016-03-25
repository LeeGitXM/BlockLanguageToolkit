/**
 * Copyright 2014-2016. ILS Automation. All rights reserved.
 * 
 * Derived from prefuse.org "TreeView" sample code.
 * https://github.com/prefuse/Prefuse
 * 
 * @author <a href="http://jheer.org">jeffrey heer</a>
 */
package com.ils.blt.client.component.recmap;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import javax.swing.BorderFactory;
import javax.swing.border.BevelBorder;

import com.ils.blt.client.component.ILSRepaintAction;
import com.ils.blt.client.component.recmap.delegate.DiagnosisDelegate;
import com.ils.blt.client.component.recmap.delegate.OutputDelegate;
import com.ils.blt.client.component.recmap.delegate.RecommendationDelegate;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.Constants;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.Action;
import prefuse.action.ActionList;
import prefuse.action.ItemAction;
import prefuse.action.animate.ColorAnimator;
import prefuse.action.animate.LocationAnimator;
import prefuse.action.animate.QualityControlAnimator;
import prefuse.action.animate.VisibilityAnimator;
import prefuse.action.assignment.ColorAction;
import prefuse.action.assignment.FontAction;
import prefuse.activity.SlowInSlowOutPacer;
import prefuse.controls.PanControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.data.tuple.TupleSet;
import prefuse.render.AbstractShapeRenderer;
import prefuse.render.DefaultRendererFactory;
import prefuse.render.EdgeRenderer;
import prefuse.util.ColorLib;
import prefuse.util.GraphicsLib;
import prefuse.util.display.DisplayLib;
import prefuse.visual.VisualItem;
import prefuse.visual.expression.InGroupPredicate;
import prefuse.visual.tuple.TableNodeItem;


/**
 * Demonstration of a node-link tree viewer
 *
 * @version 1.0
 * @author <a href="http://jheer.org">jeffrey heer</a>
 */
public class RecMapView extends Display {
	private static final long serialVersionUID = 3253162293683958367L;
	private static final String TAG = "RecMapView";
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
    
	// Groups
    private static final String GROUP_ALL = "map";
    private static final String GROUP_ALL_NODES = "map.nodes";  // Magic name, not Graph.NODES.
    private static final String GROUP_ALL_EDGES = "map.edges";  // Magic name, not Graph.EDGES.
 
    private TableLabelRenderer nodeRenderer;
    private EdgeRenderer edgeRenderer;
    private final ThreeColumnLayout columnLayout;
   
    public RecMapView(RecommendationMap recmap) {
        super(new Visualization());
        
        Dimension sz = recmap.getSize();
        RecMapDataModel model = recmap.getModel();
        
        // NOTE: Returns a VisualGraph, node/edge tables are VisualTables
        //                             node items are TableNodeItems
        m_vis.addGraph(GROUP_ALL, model.getGraph());
        
    
        setSize(sz);
        setBackground(new Color(230,228,227));
        setBorder(BorderFactory.createCompoundBorder(
                                BorderFactory.createBevelBorder(BevelBorder.RAISED), 
                                BorderFactory.createBevelBorder(BevelBorder.LOWERED)));
        
        // NOTE: No images to render.
        nodeRenderer = new TableLabelRenderer();
        nodeRenderer.setRenderType(AbstractShapeRenderer.RENDER_TYPE_DRAW_AND_FILL);
        nodeRenderer.setHorizontalAlignment(Constants.LEFT);
        nodeRenderer.setRoundedCorner(1,1);
        nodeRenderer.setVerticalPadding(3);
        nodeRenderer.setHorizontalPadding(4);

        edgeRenderer = new RecMapEdgeRenderer(Constants.EDGE_TYPE_LINE);
 
        DefaultRendererFactory rf = new DefaultRendererFactory(nodeRenderer);
        rf.add(new InGroupPredicate(GROUP_ALL_EDGES), edgeRenderer);
        m_vis.setRendererFactory(rf);
               
        // colors
        ItemAction nodeColor = new NodeColorAction(GROUP_ALL_NODES);
        ItemAction textColor = new ColorAction(GROUP_ALL_NODES,VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0));
        ItemAction strokeColor = new ColorAction(GROUP_ALL_NODES,VisualItem.STROKECOLOR, ColorLib.rgb(0,0,0));
        m_vis.putAction("strokeColor", strokeColor);
        m_vis.putAction("textColor", textColor);
        
        //ItemAction edgeColorAction = new EdgeColorAction(GROUP_ALL_EDGES,VisualItem.STROKECOLOR, ColorLib.rgb(150,255,150));
        ItemAction edgeColorAction = new EdgeColorAction(GROUP_ALL_EDGES);
        
        // quick repaint
        ActionList repaint = new ActionList();
        repaint.add(nodeColor);
        repaint.add(new ILSRepaintAction());
        m_vis.putAction("repaint", repaint);
        
        // full paint
        ActionList fullPaint = new ActionList();
        fullPaint.add(nodeColor);
        m_vis.putAction("fullPaint", fullPaint);
        
        // animate paint change
        ActionList animatePaint = new ActionList(400);
        animatePaint.add(new ColorAnimator(GROUP_ALL_NODES));
        animatePaint.add(new ILSRepaintAction());
        m_vis.putAction("animatePaint", animatePaint);

        // create a grid layout action
        columnLayout = new ThreeColumnLayout(GROUP_ALL,model.getSourceRowCount(),model.getRecommendationCount(),model.getTargetRowCount(),
        									RecMapConstants.KIND,RecMapConstants.SOURCEROW,RecMapConstants.TARGETROW);
        // Rectangle(x,y,width,height)
        columnLayout.setLayoutBounds(new Rectangle2D.Double(0.,0.,sz.width,sz.height));
        columnLayout.setLayoutAnchor(new Point2D.Double(sz.getWidth()/2.,sz.getHeight()/2.));
        m_vis.putAction("columnLayout", columnLayout);
        
        
        
        RecMapTooltipControl tooltipControl = new RecMapTooltipControl(recmap.getModel());
        tooltipControl.setDelegate(RecMapConstants.SOURCE_KIND, new DiagnosisDelegate());
        tooltipControl.setDelegate(RecMapConstants.INFO_KIND, new RecommendationDelegate());
        tooltipControl.setDelegate(RecMapConstants.TARGET_KIND, new OutputDelegate());
        addControlListener(tooltipControl);
        
        // create the filtering and layout
        ActionList filter = new ActionList();
        //filter.add(new FontAction(mapNodes, FontLib.getFont("Tahoma", 16)));
        filter.add(new FontAction(GROUP_ALL_NODES, new Font("monospaced",Font.PLAIN,16)));
        filter.add(columnLayout);
        filter.add(textColor);
        filter.add(nodeColor);
        filter.add(strokeColor);
        filter.add(edgeColorAction);
        m_vis.putAction("filter", filter);
        
        // animated transition
        ActionList animate = new ActionList(1000);
        AutoCenterAction autoCenter = new AutoCenterAction();
        animate.setPacingFunction(new SlowInSlowOutPacer());
        animate.add(autoCenter);
        animate.add(new QualityControlAnimator());
        animate.add(new VisibilityAnimator(GROUP_ALL));
        animate.add(new LocationAnimator(GROUP_ALL_NODES));
        animate.add(new ColorAnimator(GROUP_ALL_NODES));
        animate.add(new ILSRepaintAction());
        m_vis.putAction("animate", animate);
        m_vis.alwaysRunAfter("filter", "animate");


        // ------------------------------------------------
        setSize(getWidth(),getHeight());
        // initialize the display
        addControlListener(new RecMapSelector(recmap,2));    // Double-click
        addControlListener(new ZoomToFitControl());          // Control right-mouse
        addControlListener(new ZoomControl());
        addControlListener(new WheelZoomControl());
        addControlListener(new PanControl());                // Drag
        //addControlListener(new FocusControl(1, "filter"));   // Single click
       
        // ------------------------------------------------
        
        // filter graph and perform layout
        orient();
        m_vis.run("filter");

        //Rectangle2D bounds = m_vis.getBounds(map);
        Rectangle2D bounds = columnLayout.getLayoutBounds();
        log.infof("%s.constructor: visualization bounds %2.1f x %2.1f (%f,%f)",TAG,bounds.getWidth(),
                bounds.getHeight(),
                bounds.getX(),
                bounds.getY()
                );
        int margin = (int)(bounds.getWidth()/10);
        GraphicsLib.expand(bounds, margin * (int)(1/this.getScale()));
        DisplayLib.fitViewToBounds(this, bounds, RecMapConstants.ZOOM_DURATION);
        log.infof("%s.constructor: controls complete",TAG);
    }
    
    @Override
    public void setSize(int w,int h) {
    	super.setSize(w,h);
    	if( columnLayout!=null ) columnLayout.setLayoutAnchor(new Point2D.Double(w/2,h/2));
    }
    
    // ------------------------------------------------------------------------
   // Set orientation left-to-right.
    public void orient() {
            edgeRenderer.setHorizontalAlignment1(Constants.RIGHT);
            edgeRenderer.setHorizontalAlignment2(Constants.LEFT);
            edgeRenderer.setVerticalAlignment1(Constants.CENTER);
            edgeRenderer.setVerticalAlignment2(Constants.CENTER);
    }

    
    // ------------------------------------------------------------------------
   
    // ------------------------------------------------------------------------
   
    // This appears to never execute
    public class AutoCenterAction extends Action {
        private Point2D m_start = new Point2D.Double();
        private Point2D m_end   = new Point2D.Double();
        private Point2D m_cur   = new Point2D.Double();
        private int     m_bias  = 150;
        
        public void run(double frac) {
            TupleSet ts = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS);
            if ( ts.getTupleCount() == 0 )
                return;
            // Left-to-right orientation
            if ( frac == 0.0 ) {
                int xbias=0, ybias=0;
                xbias = m_bias;

                VisualItem vi = (VisualItem)ts.tuples().next();
                m_cur.setLocation(getWidth()/2, getHeight()/2);
                getAbsoluteCoordinate(m_cur, m_start);
                m_end.setLocation(vi.getX()+xbias, vi.getY()+ybias);
            } 
            else {
                m_cur.setLocation(m_start.getX() + frac*(m_end.getX()-m_start.getX()),
                                  m_start.getY() + frac*(m_end.getY()-m_start.getY()));
                panTo(m_cur);
                zoom(m_cur,0.5);
            }
            log.infof("%s.AutoCenterAction: frac %2.1f (%dx%d)",TAG,frac,getWidth(),getHeight());
        }
    }
    
    public static class NodeColorAction extends ColorAction {
    	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
        public NodeColorAction(String group) {
            super(group, VisualItem.FILLCOLOR);
        }
        @Override
        public int getColor(VisualItem item) {
        	int color = ColorLib.rgba(220,220,220,100);  // Gray
            if ( item instanceof TableNodeItem ) {
            	int kind = item.getInt(RecMapConstants.KIND);
            	if( kind==RecMapConstants.SOURCE_KIND )      color= ColorLib.rgb(204,215,235);
            	else if( kind==RecMapConstants.TARGET_KIND ) color= ColorLib.rgb(195,207,235);
            	else                                         color= ColorLib.rgb(172,185,190);
            }
            return color;
        }
        @Override
        public int getDefaultColor() {
        	return  ColorLib.rgb(198,229,229);
        }
    } 
} 

