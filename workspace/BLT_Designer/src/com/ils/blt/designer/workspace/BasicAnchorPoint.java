/**
 *   (c) 2014-2017  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.workspace;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.Shape;
import java.awt.Stroke;
import java.lang.NumberFormatException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.EnumSet;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.designer.workspace.ui.AnchorSide;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;

/**
 * Use this class to create anchor points on ProcessDiagramViews. In addition to 
 * the standard attributes, this anchor point adds side, data type.
 * It is also a PushNotification listener and provides current strokes and colors 
 * for drawing the connection.
 * 
 * This may seem like a strange choice of class from which to dispense graphical elements,
 * but it has ended up as the go-between between update notifications from the Gateway
 * and workspace connection rendering.
 */
public class BasicAnchorPoint extends AnchorPoint implements NotificationChangeListener {
	private static LoggerEx log = LogUtil.getLogger(AnchorPoint.class.getPackage().getName());
	protected final static String DEFAULT_FORMAT = "yyyy/MM/dd HH:mm:ss";
	protected final static SimpleDateFormat dateFormatter = new SimpleDateFormat(DEFAULT_FORMAT);
	// Here is our repertoire of strokes and colors ...
	// The outline strokes are black and are laid down first.
	private final Stroke signalOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_SIGNAL);
	private final Stroke truthvalueOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE);
	private final Stroke dataOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_DATA);
	private final Stroke textOutlineStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TEXT);
	
	private static final float[] DASH_PATTERN = {10f,10f};
	private static final float PHASE = 20f;
	private static final float LIMIT = 10f;   // Mitre limit
	
	private static final Stroke dataBadStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_DATA-2,BasicStroke.CAP_BUTT,BasicStroke.JOIN_ROUND,LIMIT,DASH_PATTERN,PHASE);
	private static final Stroke dataCoreStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_DATA-2);
	private static final Stroke textBadStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TEXT-2,BasicStroke.CAP_BUTT,BasicStroke.JOIN_ROUND,LIMIT,DASH_PATTERN,PHASE);
	private static final Stroke textCoreStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TEXT-2);
	private static final Stroke truthvalueBadStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE-2,BasicStroke.CAP_BUTT,BasicStroke.JOIN_ROUND,LIMIT,DASH_PATTERN,PHASE);
	private static final Stroke truthvalueCoreStroke = new BasicStroke(WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE-2);
	
	private static final UtilityFunctions fncs = new UtilityFunctions();
	private final Point anchor;
	private final Point pathLeader;
	private final Shape hotspot;
	private ConnectionType cxnType;
	private AnchorSide side;
	private final String annotation;
	private boolean isEmpty = true;
	private boolean isGood = true;
	private boolean isNan = false;
	private boolean allowMultiple = true;
	private TruthValue theTruth = TruthValue.UNSET;
	private QualifiedValue lastValue = null;
	
	/**
	 * 
	 * @param id
	 * @param block
	 * @param ttype termination type
	 * @param ctype connection type
	 * @param anch the spot at which the connection stops
	 * @param leader the leader is a point about 10 pixels from the anchor, used for drawing the connection.
	 * @param spot
	 * @param multiple
	 * @param note a String annotation to be drawn next to the connection (inside the block)
	 */
	
	public BasicAnchorPoint(Object id, Block block, AnchorType ttype, ConnectionType ctype, Point anch, Point leader, Shape spot,boolean multiple,String note) {
		super(id, block, EnumSet.of(ttype));
		this.anchor = anch;
		this.pathLeader = leader;
		this.hotspot = spot;
		this.annotation = note;
		this.allowMultiple = multiple;
		// Default behavior for side. This can be updated.
		if( ttype==AnchorType.Origin) side = AnchorSide.RIGHT;
		else side = AnchorSide.LEFT;
		cxnType = ctype;
	}

	public boolean allowMultipleConnections() { return allowMultiple; }
	public Point getAnchor() { return anchor; }
	public String getAnnotation() { return annotation; }
	public ConnectionType getConnectionType() { return cxnType; }
	public Shape getHotSpot() { return hotspot; }
	public Point getPathLeader() { return pathLeader; }
	public AnchorSide getSide() {return side;}
	public void setConnectionType(ConnectionType type) { this.cxnType = type; }
	public void setSide(AnchorSide side) {this.side = side;}
	

	// Methods for workspace connection rendering
	/**
	 * @return the stroke that covers the middle of the connection.
	 *         The stroke varies by data type and quality.
	 *         This should never be called for a signal.
	 */
	public Stroke getCoreStroke() {
		return getCoreStroke(cxnType);
	}
	public Stroke getCoreStroke(ConnectionType ctype) {
		if( ctype.equals(ConnectionType.DATA)) {
			if( isGood ) {
				if( isNan || isEmpty ) return dataBadStroke;
				else return dataCoreStroke;
			}
			else return dataBadStroke;
		}
		else if( ctype.equals(ConnectionType.TRUTHVALUE)) {
			if( isEmpty )
				return truthvalueBadStroke;
			else if( isGood )
				return truthvalueCoreStroke;
			else
				return truthvalueBadStroke;
		}
		else   // TEXT
			if( isEmpty ) return textBadStroke;
			if( isGood )  return textCoreStroke;
			else return textBadStroke;
	}
	/**
	 * @return the color that covers the middle of the connection.
	 *         The stroke varies by data type, quality and value.
	 */
	public Color getCoreColor() {
		return getCoreColor(cxnType);
	}

	public Color getCoreColor(ConnectionType cType) {
		if( isEmpty ) 
			return WorkspaceConstants.CONNECTION_FILL_EMPTY; // Dashed
		else if( !isGood )
			return WorkspaceConstants.CONNECTION_FILL_BAD;  // Dashed
		else if( isNan )
			return WorkspaceConstants.CONNECTION_FILL_NAN;
		else if( cType.equals(ConnectionType.DATA)) {
			return WorkspaceConstants.CONNECTION_FILL_DATA;
		}
		else if( cType.equals(ConnectionType.TRUTHVALUE))
			if( theTruth.equals(TruthValue.TRUE))
				return WorkspaceConstants.CONNECTION_FILL_TRUE;
			else if( theTruth.equals(TruthValue.FALSE))
				return WorkspaceConstants.CONNECTION_FILL_FALSE;
			else
				return WorkspaceConstants.CONNECTION_FILL_UNKNOWN;
		else if( cType.equals(ConnectionType.ANY))
			return WorkspaceConstants.CONNECTION_FILL_ANY;
		else
			return WorkspaceConstants.CONNECTION_FILL_TEXT;
	}
	
	/**
	 * @return the last value received at this anchor point.
	 */
	public QualifiedValue getLastValue() { return lastValue; }
	/**
	 * @return the stroke that that is the connection background.
	 *         Ultimately only its edges show as outines.
	 */
	public Stroke getOutlineStroke() {
		if( cxnType.equals(ConnectionType.DATA))
			return dataOutlineStroke;
		else if( cxnType.equals(ConnectionType.SIGNAL))
			return signalOutlineStroke;
		else if( cxnType.equals(ConnectionType.TRUTHVALUE))
			return truthvalueOutlineStroke;
		else 
			return textOutlineStroke;   // TEXT and ANY
	}
	/**
	 * @return the color of the outlines for the main stroke.
	 */
	public Color getOutlineColor() {
		return WorkspaceConstants.CONNECTION_BACKGROUND;
	}
	
	public void reset() {
		isGood = true;
		isEmpty = true;
		isNan   = false;
		theTruth = TruthValue.UNSET;
		lastValue = null;
	}

	/**
	 * Receive an event from the Gateway. Ignore a binding change,
	 */
	@Override
	public void bindingChange(String value) {}
	@Override
	public void diagramAlertChange(long resId, String alerting) {}
	/**
	 * Receive an event from the Gateway. Use the information to color
	 * the connection. By convention we use the Origin as it is already
	 * used to determine the connection type. NOTE: registration for 
	 * events is handled by the workspace on opening.
	 */
	@Override
	public void valueChange(QualifiedValue value) {
		//  This is the most recent value to pass into the connection.
		// Use it to determine the fill color
		if(value.getValue()==null) return;
		
		log.tracef("BasicAnchorPoint.valueChange: received %s.",value.getValue().toString());
		isGood = value.getQuality().isGood();
		isEmpty = false;
		isNan = false;
		if(value.getValue().toString().equalsIgnoreCase("UNSET")) {
			isEmpty=true;
			theTruth = TruthValue.UNSET;
		}
		
		if( !isEmpty && isGood ) {
//			if( cxnType.equals(ConnectionType.TRUTHVALUE)) {  
				theTruth = fncs.qualifiedValueAsTruthValue(value);   // set this anyway, just in case the data type is 'any'
//			}
//				else if( cxnType.equals(ConnectionType.DATA)) {
				if( cxnType.equals(ConnectionType.DATA)) {
				// Dates, doubles in a data path are automatically OK
				// If we have a string, check its format
				if( value.getValue() instanceof String ) {
					try {
						dateFormatter.parse(value.getValue().toString());
					}
					catch(ParseException pe) {
						try {
							isNan = Double.isNaN(fncs.coerceToDouble(value.getValue()));
						}
						catch(NumberFormatException nfe) {
							isNan = true;
						}
					}
					catch(NumberFormatException nfe) {
						log.warnf("BasicAnchorPoint.valueChange: received %s - could not format as double or date",value.getValue().toString());
						isNan = true;
					}
				}
			}
			// Any, signal, text
			else {
				isEmpty = false;   // Even an empty string is OK
			}
		}
		lastValue = value;
		((ProcessBlockView)getBlock()).recordLatestValue(id.toString(),value);
	}
	@Override
	public void watermarkChange(String watermark) {}

	public boolean allowConnectionType(ConnectionType connectionType) {
		boolean ret = false;
		if ((connectionType == ConnectionType.ANY) || (getConnectionType() == ConnectionType.ANY) || (getConnectionType() == connectionType)) {
			ret = true;
		}
		return ret;
	}
}
