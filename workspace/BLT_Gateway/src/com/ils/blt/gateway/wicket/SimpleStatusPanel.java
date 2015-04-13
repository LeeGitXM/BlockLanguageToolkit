package com.ils.blt.gateway.wicket;

import java.util.List;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

import com.ils.blt.gateway.engine.ProcessDiagram;

public class SimpleStatusPanel extends Panel {
	private static final long serialVersionUID = 4204748023293522204L;


	public SimpleStatusPanel(String id) {
		super(id);
		
        // This is the static view of the diagram
		IModel<List<ProcessDiagram>> diagramListModel = new SimpleDiagramListModel();
		add(new ListView<ProcessDiagram>("diagrams",diagramListModel) {
			private static final long serialVersionUID = 3537127058517061095L;
			protected void populateItem(ListItem<ProcessDiagram> item) {
				ProcessDiagram diagram = item.getModelObject();
				item.add(new Label("name",diagram.getName()));
			}
		});
	}
}
