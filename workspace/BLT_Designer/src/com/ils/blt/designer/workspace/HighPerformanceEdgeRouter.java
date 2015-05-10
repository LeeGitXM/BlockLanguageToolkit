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
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.routing.AbstractEdgeRouter;
 
/**
 * Created by carl.gould on 1/23/2015.
 */
public class HighPerformanceEdgeRouter extends AbstractEdgeRouter {
	private static final String TAG = "HighPerformanceEdgeRouter";
	private static LoggerEx log = LogUtil.getLogger(HighPerformanceEdgeRouter.class.getPackage().getName());
	private Map<UUID, Rectangle> rectangles;
	
	@Override
	public void setup(Map<UUID, Rectangle> obstacles) {
		log.infof("%s.setup ...",TAG);
		this.rectangles = obstacles;
	}
 
	@Override
	protected Point getBlockLocation(UUID id) {
		log.infof("%s.getBlockLocation ...",TAG);
		return rectangles.get(id).getLocation();
	}
	@Override
	public Path2D route(AnchorPoint start, Point end) {
		log.infof("%s.route (anchor-point) ...",TAG);
		return super.route(start, end);
	}
	@Override
	public Path2D route(AnchorPoint start, AnchorPoint end) {
		log.infof("%s.route (anchor-anchor) ...",TAG);
		return super.route(start, end);
	}
	
	@Override
	protected List<Point> route(Point start, Point end, Point trueStart, Point trueEnd) {
		log.infof("%s.route ...",TAG);
		List<Point> points = new ArrayList<Point>(2);
		if (start.x == end.x || start.y == end.y) {
			// straight line
			points.add(start);
			points.add(end);
		} 
		else {
			// basic ortho line
			Point mid = round(centerPoint(start, end));
			points.add(start);
			points.add(new Point(mid.x, start.y));
			points.add(new Point(mid.x, end.y));
			points.add(end);
		}
		return points;
	}
 
	@Override
	public List<Path2D> routeAll(Collection<Connection> connections) {
		log.infof("%s.routeAll ...",TAG);
		return super.routeAll(connections);
	}
 
	@Override
	public void paintDebug(Graphics2D g) {
		log.infof("%s.paintDebug ...",TAG);
	}
}
