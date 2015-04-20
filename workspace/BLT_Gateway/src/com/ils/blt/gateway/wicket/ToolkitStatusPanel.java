package com.ils.blt.gateway.wicket;

import java.util.ArrayList;
import java.util.List;

import javatests.Foo;

import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.tree.AbstractTree;
import org.apache.wicket.extensions.markup.html.repeater.tree.DefaultNestedTree;
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
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessNode;

/**
 * @see http://www.wicket-library.com/wicket-examples/nested/wicket/bookmarkable/org.apache.wicket.examples.ajax.builtin.tree.TreeTablePage?0
 */
public class ToolkitStatusPanel extends Panel {
	public static final long serialVersionUID = 4204748023293522204L;
    private ProcessNodeProvider provider = new ProcessNodeProvider();

	public ToolkitStatusPanel(String id) {
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
        
        AbstractTree<ProcessNode> tree = createTree("tree",provider);
        form.add(tree);
     
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
	
    private AbstractTree<ProcessNode> createTree(String nid,ProcessNodeProvider prov) {
    	AbstractTree<ProcessNode> atree = new DefaultNestedTree<ProcessNode>(nid, prov) {
    		private static final long serialVersionUID = 5798665408737036777L;
    		/**
    		 * To use a custom component for the representation of a node's content we would
    		 * override this method.
    		 */
    		@Override
    		protected Component newContentComponent(String cid, IModel<ProcessNode> node) {
    			return super.newContentComponent(cid, node);
    		}
    	};
    	return atree;
    }
    
    
    // Create the columns for our tabluar view
    // Node folder, name, state, transitions
    // Hierarchy is project-application-(folder)-family-(folder)-diagram-block
    private List<IColumn<ProcessNode, String>> createColumns() {
        List<IColumn<ProcessNode, String>> columns = new ArrayList<>();
        columns.add(new TreeColumn<ProcessNode, String>(Model.of("Tree")));
        columns.add(new PropertyColumn<ProcessNode, String>(Model.of("Name"),"Name") {
        	private static final long serialVersionUID = 1L;
        	@Override
            public void populateItem(Item<ICellPopulator<ProcessNode>> cellItem, String componentId,IModel<ProcessNode> rowModel) {
                NodeModel<ProcessNode> nodeModel = (NodeModel<ProcessNode>)rowModel;
                cellItem.add(new Label(componentId, "" + nodeModel.getDepth()));
            }

            @Override
            public String getCssClass() { return "number";}
        });
        columns.add(new PropertyColumn<ProcessNode, String>(Model.of("State"), "State") {
        	private static final long serialVersionUID = 1L;
        	@Override
            public void populateItem(Item<ICellPopulator<ProcessNode>> cellItem, String componentId,IModel<ProcessNode> rowModel) {
                NodeModel<ProcessNode> nodeModel = (NodeModel<ProcessNode>)rowModel;
                cellItem.add(new Label(componentId, "" + nodeModel.getDepth()));
            }

            @Override
            public String getCssClass() { return "number";}
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
	
	/*
	protected AbstractTree<ProcessNode> createTree(ProcessNodeProvider prov,IModel<Set<ProcessNode>> state) {
		final NestedTreePage page = new NestedTreePage(state);
        tree = new NestedTree<ProcessNode>("tree", prov, state) {
            private static final long serialVersionUID = 1L;

            @Override
            protected Component newContentComponent(String id, IModel<ProcessNode> model)
            {
                //return page.newContentComponent(id, model);
            	return null;
            }
        };
        return tree;
    }
    
  
        tree = new TreeTable("treeTable", createTreeModel(), columns);
        tree.getTreeState().setAllowSelectMultiple(true);
        add(tree);
        tree.getTreeState().collapseAll();
        tree = createTree(provider,new ProcessNodeExpansionModel());
        
        // Set the behavior of the tree
        tree.add(new Behavior()  {
            private static final long serialVersionUID = 1L;
            public HumanTheme theme = new HumanTheme();

            @Override
            public void onComponentTag(Component component, ComponentTag tag) {
                theme.onComponentTag(component, tag);
            }

            @Override
            public void renderHead(Component component, IHeaderResponse response) {
                theme.renderHead(component, response);
            }
        });
   
       
	
	private class ProcessNodeExpansionModel extends AbstractReadOnlyModel<Set<ProcessNode>> {
		private static final long serialVersionUID = -4327444737693656454L;

		@Override
        public Set<ProcessNode> getObject() {
            return ProcessNodeExpansion.get();
        }
    }
    */
}
