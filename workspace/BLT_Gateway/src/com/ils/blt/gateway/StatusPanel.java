package com.ils.blt.gateway;

import java.util.List;

import org.apache.wicket.Application;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.AbstractReadOnlyModel;

import com.ils.blt.gateway.engine.ProcessDiagram;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

public class StatusPanel extends Panel {
	private static final long serialVersionUID = 4204748023293522204L;


	public StatusPanel(String id) {
		super(id);
		final List<ProcessDiagram> diagrams = getHook().getStatusData().getDiagrams();
		add(new Label("diagramCount",new AbstractReadOnlyModel() {
			@Override
			public Object getObject() {
				return diagrams.size();
			}
		}));
		
		for(final ProcessDiagram diagram:diagrams) {
			add(new Label("diagram",new AbstractReadOnlyModel() {
				@Override
				public Object getObject() {
					return diagram.getName();
				}
			}));
		}
	}
	
	
	private BLTGatewayHook getHook() {
		GatewayContext ctx = (GatewayContext)Application.get();
		return BLTGatewayHook.get(ctx);
	}
}
