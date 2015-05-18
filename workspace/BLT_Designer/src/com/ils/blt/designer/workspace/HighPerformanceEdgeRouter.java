/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;


import static com.inductiveautomation.ignition.client.util.gui.DrawingUtilities.centerPoint;
import static com.inductiveautomation.ignition.client.util.gui.DrawingUtilities.round;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.routing.AbstractEdgeRouter;
 
/**
 * Created by carl.gould on 1/23/2015.
 */
public class HighPerformanceEdgeRouter extends AbstractEdgeRouter {
	private static final String TAG = "HighPerformanceEdgeRouter";
	private static LoggerEx log = LogUtil.getLogger(HighPerformanceEdgeRouter.class.getPackage().getName());
	private Map<UUID, Rectangle> rectangles;
	private static final int BLOCK_SIZE = 100;    // Typical
	private static final int ROUNDING_RADIUS = 8;
	
	@Override
	public void setup(Map<UUID, Rectangle> obstacles) {
		log.infof("%s.setup ... %d blocks",TAG,obstacles.size());
		this.rectangles = obstacles;
	}
 
	/**
	 * This gets called several times during the routing of each point.
	 */
	@Override
	protected Point getBlockLocation(UUID id) {
		return rectangles.get(id).getLocation();
	}

	/**
	 * This gets called right after the setup() method, once per creation
	 * of the diagram. Need to create routes for every orientation. Fortunately
	 * in the toolkit nearly every connection goes horizontally from left to
	 * right. That is the vast majority of blocks have inputs on the left and 
	 * outputs on the right. Only a few have inputs on top or outputs on the 
	 * bottom.
	 */
	@Override
	protected List<Point> route(Point start, Point end, Point trueStart, Point trueEnd) {
		//log.infof("%s.route ...",TAG);
		List<Point> points = new ArrayList<Point>(2);
		if (start.x == end.x || start.y == end.y) {
			// straight line. Nothing more required
			points.add(start);	
			points.add(end);
		} 
		else {
			// Determine the orientations - we're only allowing connections from right or bottom
			// to left or top.
			if( start.y==trueStart.y ) {
				if(end.y==trueEnd.y ) {
					drawRightToLeft(start,end,points);
				}
				else {
					drawRightToTop(start,end,points);
				}
			}
			else {
				if(end.y==trueEnd.y ) {
					drawBottomToLeft(start,end,points);
				}
				else {
					drawBottomToTop(start,end,points);
				}
			}
		}
		
		return points;
	}
 
	/**
	 * This gets called right after the setup() method, once per creation
	 * of the diagram. It transforms the connections to window units,
	 * calls route() on each connections, then calls the pathmaker
	 * to create 2D paths.
	 */
	@Override
	public List<Path2D> routeAll(Collection<Connection> connections) {
		log.debugf("%s.routeAll ...",TAG);
		return super.routeAll(connections);
	}
 
	@Override
	public void paintDebug(Graphics2D g) {}
	
	// =================================== Draw the various orientations ==========================
	// This is by far the most common direction
	private void drawRightToLeft(Point start,Point end,List<Point>points) {
		if( start.x < end.x - ROUNDING_RADIUS ) {
			// basic ortho line
			Point mid = round(centerPoint(start, end));
			points.add(start);
			points.add(new Point(mid.x, start.y));
			points.add(new Point(mid.x, end.y));
			points.add(end);
		}
		else {
			// Have to snake back to make route
			Point mid = round(centerPoint(start, end));
			points.add(start);
			points.add(new Point(start.x+ROUNDING_RADIUS, start.y));
			points.add(new Point(start.x+ROUNDING_RADIUS, mid.y));
			points.add(new Point(end.x-ROUNDING_RADIUS, mid.y));
			points.add(new Point(end.x-ROUNDING_RADIUS, end.y));
			points.add(end);
		}
	}
	private void drawRightToTop(Point start,Point end,List<Point>points) {
		// TODO
		points.add(start);
		points.add(new Point(end.x, start.y));
		points.add(end);
	}
	private void drawBottomToLeft(Point start,Point end,List<Point>points) {
		// TODO
		points.add(start);	
		points.add(new Point(start.x, end.y));
		points.add(end);
	}
	private void drawBottomToTop(Point start,Point end,List<Point>points) {
		if( start.y < end.y - ROUNDING_RADIUS ) {
			// basic ortho line
			Point mid = round(centerPoint(start, end));
			points.add(start);
			points.add(new Point(start.x, mid.y));
			points.add(new Point(end.x, mid.y));
			points.add(end);
		}
		else {
			// Have to snake back to make route
			Point mid = round(centerPoint(start, end));
			points.add(start);
			points.add(new Point(start.x, start.y+ROUNDING_RADIUS));
			points.add(new Point(mid.x, start.y+ROUNDING_RADIUS));
			points.add(new Point(mid.x, end.y-ROUNDING_RADIUS));
			points.add(new Point(end.x, end.y-ROUNDING_RADIUS));
			points.add(end);
		}
	}
}
