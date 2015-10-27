package com.ils.blt.gateway.wicket;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.HeadersToolbar;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.NoRecordsToolbar;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.tree.AbstractTree;
import org.apache.wicket.extensions.markup.html.repeater.tree.TableTree;
import org.apache.wicket.extensions.markup.html.repeater.tree.table.NodeModel;
import org.apache.wicket.extensions.markup.html.repeater.tree.table.TreeColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.OddEvenItem;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessNode;
import com.ils.blt.gateway.wicket.content.Content;
import com.ils.blt.gateway.wicket.content.EditableFolderContent;
import com.ils.blt.gateway.wicket.content.ProcessTreeBehavior;

/**
 * @see http://www.wicket-library.com/wicket-examples/nested/wicket/bookmarkable/org.apache.wicket.examples.ajax.builtin.tree.TreeTablePage?0
 */
public class ToolkitStatusPanel extends Panel {
	public static final long serialVersionUID = 4204748023293522204L;
    private ProcessNodeProvider provider = new ProcessNodeProvider();
    private final Content content;

	public ToolkitStatusPanel(String id) {
		super(id);
		this.content = new EditableFolderContent();
		
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
        
        AbstractTree<ProcessNode> tree = createTree("tree",provider);
        // Set the behavior of the tree
        tree.add(new ProcessTreeBehavior());  // Set the behavior of the tree
        form.add(tree);
     
        // This is the static view of the diagram
		IModel<List<ProcessDiagram>> diagramListModel = new SimpleDiagramListModel();
		add(new ListView<ProcessDiagram>("diagrams",diagramListModel) {
			private static final long serialVersionUID = 3537127058517061095L;
			protected void populateItem(ListItem<ProcessDiagram> item) {
				ProcessDiagram diagram = item.getModelObject();
				String label = String.format("%3d.%4d: %s", diagram.getProjectId(),diagram.getResourceId(),diagram.getName());
				item.add(new Label("name",label));
			}
		});
	}
	
    private AbstractTree<ProcessNode> createTree(String markup,ProcessNodeProvider prov ) {
        List<IColumn<ProcessNode, String>> columns = createColumns();
        IModel<Set<ProcessNode>> state = new ProcessNodeExpansionModel();

        final TableTree<ProcessNode, String> tree = new TableTree<ProcessNode, String>(markup, columns, prov, Integer.MAX_VALUE, state) {
            private static final long serialVersionUID = 1L;

            @Override
            protected Component newContentComponent(String id, IModel<ProcessNode> model) {
                return ToolkitStatusPanel.this.newContentComponent(id, this, model);
            }

            @Override
            protected Item<ProcessNode> newRowItem(String id, int index, IModel<ProcessNode> model) {
                return new OddEvenItem<ProcessNode>(id, index, model);
            }
        };
        tree.getTable().addTopToolbar(new HeadersToolbar<String>(tree.getTable(), null));
        tree.getTable().addBottomToolbar(new NoRecordsToolbar(tree.getTable()));
        return tree;
    }
    
    @Override
    public void detachModels() {
        super.detachModels();
    }
    
    
    // Create the columns for our tabluar view
    // Node folder, name, state, transitions
    // Hierarchy is project-application-(folder)-family-(folder)-diagram-block
    private List<IColumn<ProcessNode, String>> createColumns() {
        List<IColumn<ProcessNode, String>> columns = new ArrayList<>();
        columns.add(new TreeColumn<ProcessNode, String>(Model.of("Node")));
        columns.add(new PropertyColumn<ProcessNode, String>(Model.of("Name"),"Name") {
        	private static final long serialVersionUID = 1L;
        	@Override
            public void populateItem(Item<ICellPopulator<ProcessNode>> cellItem, String componentId,IModel<ProcessNode> rowModel) {
                NodeModel<ProcessNode> nodeModel = (NodeModel<ProcessNode>)rowModel;
                cellItem.add(new Label(componentId, "" + nodeModel.getDepth()));
            }

            @Override
            public String getCssClass() { return "text";}
        });
        columns.add(new PropertyColumn<ProcessNode, String>(Model.of("State"), "State") {
        	private static final long serialVersionUID = 1L;
        	@Override
            public void populateItem(Item<ICellPopulator<ProcessNode>> cellItem, String componentId,IModel<ProcessNode> rowModel) {
                NodeModel<ProcessNode> nodeModel = (NodeModel<ProcessNode>)rowModel;
                cellItem.add(new Label(componentId, "" + nodeModel.getDepth()));
            }

            @Override
            public String getCssClass() { return "text";}
        });
        
        columns.add(new PropertyColumn<ProcessNode, String>(Model.of("Transitions"), "Transitions"){
        	private static final long serialVersionUID = 1L;
        	@Override
            public void populateItem(Item<ICellPopulator<ProcessNode>> cellItem, String componentId,IModel<ProcessNode> rowModel) {
                NodeModel<ProcessNode> nodeModel = (NodeModel<ProcessNode>)rowModel;
                cellItem.add(new Label(componentId, "" + nodeModel.getDepth()));
            }

            @Override
            public String getCssClass() { return "number";}
        });
        return columns;
    }
    
    private Component newContentComponent(String id, TableTree<ProcessNode, String> ttree, IModel<ProcessNode> model) {
        return content.newContentComponent(id, ttree, model);
    }
    
    
	private class ProcessNodeExpansionModel extends AbstractReadOnlyModel<Set<ProcessNode>> {
		private static final long serialVersionUID = 4327444737693656454L;

		@Override
        public Set<ProcessNode> getObject() {
            return ProcessNodeExpansion.get();
        }
    }
  
}
