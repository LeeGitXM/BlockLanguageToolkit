package com.ils.blt.gateway.wicket;

import java.util.List;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

import com.ils.blt.gateway.engine.ProcessDiagram;

public class StatusPanel extends Panel {
	private static final long serialVersionUID = 4204748023293522204L;


	public StatusPanel(String id) {
		super(id);
		
		Form<Void> form = new Form<Void>("form");
        add(form);
        form.add(new Link<Void>("expandAll") {
        	private static final long serialVersionUID = 1L;
        	@Override
            public void onClick() {
                ProcessNodeExpansion.get().expandAll();
            }
        });
        form.add(new Link<Void>("collapseAll") {
        	private static final long serialVersionUID = 1L;
        	@Override
        	public void onClick() {
        		ProcessNodeExpansion.get().collapseAll();
        	}
        });
        
        form.add(new Button("clear") {
            private static final long serialVersionUID = 1L;
            @Override
            public void onSubmit() {
            }
        });
        form.add(new Button("refresh") {
            private static final long serialVersionUID = 1L;
            @Override
            public void onSubmit() {
            }
        });
        
		
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
