/*
 * Create a "human" behavior for our tree.
*/

package com.ils.blt.gateway.wicket.content;

import org.apache.wicket.Component;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.extensions.markup.html.repeater.tree.theme.HumanTheme;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.head.IHeaderResponse;

public class ProcessTreeBehavior extends Behavior {
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
}