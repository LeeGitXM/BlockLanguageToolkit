package com.ils.blt.client.component;

import java.awt.geom.Rectangle2D;
import java.util.Iterator;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.action.layout.Layout;
import prefuse.data.Node;
import prefuse.data.tuple.TupleSet;
import prefuse.visual.VisualItem;


/**
 * Based on GridLayout. This layout is specific to a grid containing:
 *    1) Source column
 *    2) Target column
 *    3) Link column - middle column has edges to source and target. 
 *  
 */
public class ThreeColumnLayout extends Layout {
	private static final String TAG = "ThreeColumnLayout";
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	// Column types
	public static final int SOURCE_KIND      = 0;
	public static final int LINK_KIND        = 1;
	public static final int TARGET_KIND      = 2;

	protected  int nrows;
    protected  int ncols = 3;
    protected  int nrows1;
    protected  int nrows2;
    protected  int nrows3;
    protected  String columnColumn;
    protected  String sourceRefColumn;
    protected  String targetRefColumn;
    protected boolean analyze = false;
    
    /**
     * Create a new GridLayout without preset dimensions. The layout will
     * attempt to analyze an input graph to determine grid parameters.
     * @param group the data group to layout. In this automatic grid
     * analysis configuration, the group <b>must</b> resolve to a set of
     * graph nodes.
     */
    /*
    public ThreeColumnLayout(String group) {
        super(group);
        analyze = true;
    }
    */
    /**
     * Create a new ThreeColumnLayout using the specified grid dimensions.
     * @param group the data group to layout
     * @param rows1 the number of rows in the source column
     * @param rows2 the number of rows in the link column
     * @param rows3 the number of rows in the target column
     * @param col name of the item column that contains the column index
     * @param sourceCol name of the item column that contains the sourceReference
     * @param targetCol name of the item column that contains the targetReference
     */
    public ThreeColumnLayout(String group, int rows1, int rows2, int rows3, String col, String sourceCol, String targetCol) {
        super(group);
        nrows1 = rows1;
        nrows2 = rows2;
        nrows3 = rows3;
        int maxrows = 2;  // Absolute minimum
        if( rows1>maxrows) maxrows = rows1;
        if( rows2>maxrows) maxrows = rows2;
        if( rows3>maxrows) maxrows = rows3;
        nrows = maxrows;
        columnColumn = col;
        sourceRefColumn = sourceCol;
        targetRefColumn = targetCol;
        analyze = false;
        log.infof("%s.constructor group %s is %dx%d nodes (%s)",TAG,m_group,ncols,nrows,(isEnabled()?"ENABLED":"DISABLED"));
    }
    
    /**
     * @see prefuse.action.Action#run(double)
     */
    public void run(double frac) {
        Rectangle2D b = getLayoutBounds();
        double bx = b.getMinX(), by = b.getMinY();
        double w = b.getWidth(), h = b.getHeight();
        
        TupleSet ts = m_vis.getGroup(m_group);
        log.infof("%s.run group %s has %d nodes",TAG,m_group,ts.getTupleCount());
        int m = nrows, n = ncols;
        if ( analyze ) {
            int[] d = analyzeGraphGrid(ts);
            m = d[0]; n = d[1];
        }
        
        @SuppressWarnings("rawtypes")
		Iterator iter = ts.tuples();
        // layout grid contents
        for ( int i=0; iter.hasNext() && i < m*n; ++i ) {
        	Object next = iter.next();
        	if( next instanceof Node ) {
        		VisualItem item = (VisualItem)next;
                item.setVisible(true);
                double x = bx + w*((i%n)/(double)(n-1));
                double y = by + h*((i/n)/(double)(m-1));
                setX(item,null,x);
                setY(item,null,y);
        	}
            
        }
        // set left-overs invisible
        while ( iter.hasNext() ) {
            VisualItem item = (VisualItem)iter.next();
            item.setVisible(false);
        }
    }
    
    /**
     * Analyzes a set of nodes to try and determine grid dimensions. Currently
     * looks for the edge count on a node to drop to 2 to determine the end of
     * a row.
     * @param ts TupleSet ts a set of nodes to analyze. Contained tuples
     * <b>must</b> implement be Node instances.
     * @return a two-element int array with the row and column lengths
     */
    public static int[] analyzeGraphGrid(TupleSet ts) {
        // TODO: more robust grid analysis?
        int m, n;
        Iterator iter = ts.tuples(); iter.next();
        for ( n=2; iter.hasNext(); n++ ) {
        	Object next = iter.next();
        	if( next instanceof Node ) {
        		Node nd = (Node)next;
                if ( nd.getDegree() == 2 )
                    break;
        	}
        }
        m = ts.getTupleCount() / n;
        return new int[] {m,n};
    }
    
    /**
     * Get the number of grid columns.
     * @return the number of grid columns
     */
    public int getNumCols() {
        return ncols;
    }
    
    /**
     * Set the number of grid columns.
     * @param cols the number of grid columns to use
     */
    public void setNumCols(int cols) {
        this.ncols = cols;
    }
    
    /**
     * Get the number of grid rows.
     * @return the number of grid rows
     */
    public int getNumRows() {
        return nrows;
    }
    
    /**
     * Set the number of grid rows.
     * @param rows the number of grid rows to use
     */
    public void setNumRows(int rows) {
        this.nrows = rows;
    }
    
} // end of class GridLayout
