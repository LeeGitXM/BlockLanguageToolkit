/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;


import static com.inductiveautomation.ignition.client.util.gui.DrawingUtilities.centerPoint;
import static com.inductiveautomation.ignition.client.util.gui.DrawingUtilities.round;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.inductiveautomation.ignition.designer.blockandconnector.routing.AbstractEdgeRouter;
 
/**
 * Created by carl.gould on 1/23/2015.
 */
public class HighPerformanceEdgeRouter extends AbstractEdgeRouter {
 
	private Map<UUID, Rectangle> rectangles;
 
	@Override
	public void setup(Map<UUID, Rectangle> obstacles) {
		this.rectangles = obstacles;
	}
 
	@Override
	protected Point getBlockLocation(UUID id) {
		return rectangles.get(id).getLocation();
	}
 
	@Override
	protected List<Point> route(Point start, Point end, Point trueStart, Point trueEnd) {
		List<Point> points = new ArrayList<Point>(2);
		if (start.x == end.x || start.y == end.y) {
			// straight line
			points.add(start);
			points.add(end);
		} else {
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
	public void paintDebug(Graphics2D g) {
 
	}
}
