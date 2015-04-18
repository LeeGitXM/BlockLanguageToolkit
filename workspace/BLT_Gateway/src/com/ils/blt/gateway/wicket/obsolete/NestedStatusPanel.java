/*
 * The code below was based on Wicket Examples - Advanced Tree Page.
 * 
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ils.blt.gateway.wicket.obsolete;

import java.util.Set;

import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.tree.AbstractTree;
import org.apache.wicket.extensions.markup.html.repeater.tree.NestedTree;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.model.IModel;

import com.ils.blt.gateway.engine.ProcessNode;
import com.ils.blt.gateway.wicket.ProcessNodeExpansion;
import com.ils.blt.gateway.wicket.ProcessNodeProvider;

public class NestedStatusPanel extends Panel {
	private static final long serialVersionUID = 4204748023293522204L;
	private AbstractTree<ProcessNode> tree;
	private ProcessNodeProvider provider = new ProcessNodeProvider();
	
	public NestedStatusPanel(String id) {
		super(id);
		
		Form<Void> form = new Form<Void>("form");
        add(form);
        
        /*
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
        
        form.add(tree);
        
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
        */
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
	}
	
	protected AbstractTree<ProcessNode> createTree(ProcessNodeProvider prov,IModel<Set<ProcessNode>> state) {
		final NestedTreePage page = new NestedTreePage(state);
        tree = new NestedTree<ProcessNode>("tree", prov, state) {
            private static final long serialVersionUID = 1L;

            @Override
            protected Component newContentComponent(String id, IModel<ProcessNode> model)
            {
                return page.newContentComponent(id, model);
            }
        };
        return tree;
    }
	
	private class ProcessNodeExpansionModel extends AbstractReadOnlyModel<Set<ProcessNode>> {
		private static final long serialVersionUID = -4327444737693656454L;

		@Override
        public Set<ProcessNode> getObject() {
            return ProcessNodeExpansion.get();
        }
    }
}
