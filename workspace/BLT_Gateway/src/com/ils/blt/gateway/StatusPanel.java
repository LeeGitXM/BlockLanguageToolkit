package com.ils.blt.gateway;

import org.apache.wicket.Application;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.AbstractReadOnlyModel;

import com.inductiveautomation.ignition.gateway.model.GatewayContext;

public class StatusPanel extends Panel {
	public StatusPanel(String id) {
		super(id);
		add(new Label("diagramCount",new AbstractReadOnlyModel() {

			@Override
			public Object getObject() {
				// TODO Auto-generated method stub
				return getHook().getStatusData().getDialogCount();
			}
		}));
	}
	
	
	private BLTGatewayHook getHook() {
		GatewayContext ctx = (GatewayContext)Application.get();
		return BLTGatewayHook.get(ctx);
	}
}
