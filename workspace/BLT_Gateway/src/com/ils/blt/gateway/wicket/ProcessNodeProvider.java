package com.ils.blt.gateway.wicket;

import java.io.Serializable;
import java.util.Iterator;

import org.apache.wicket.extensions.markup.html.repeater.tree.ITreeProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessNode;
import com.ils.blt.gateway.engine.RootNode;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * A provider of {@link ProcessNode}s.
 * 
 * This class is used to generate the wicket view of the node tree 
 * 
 * 
 * @author Sven Meier
 */
public class ProcessNodeProvider implements ITreeProvider<ProcessNode> {
    private static final long serialVersionUID = 1L;
    private static ModelManager modelManager = BlockExecutionController.getInstance().getDelegate();

    /**
     * Construct.
     */
    public ProcessNodeProvider() {
    	
    }

    /**
     * Nothing to do.
     */
    @Override
    public void detach() {
    }

    /**
     * The "roots" are the nodes representing projects.
     * They are of type ProjectNode.
     */
    @Override
    public Iterator<ProcessNode> getRoots() {
    	RootNode root = RootNode.getInstance();
    	return root.getChildren().iterator();
    }

    @Override
    public boolean hasChildren(ProcessNode node) {
        return node.getChildren().isEmpty();
    }

    @Override
    public Iterator<ProcessNode> getChildren(final ProcessNode node) {
        return node.getChildren().iterator();
    }

    /**
     * Creates a process node model
     */
    @Override
    public IModel<ProcessNode> model(ProcessNode node) {
        return new ProcessNodeModel(node);
    }

    /**
     * A {@link Model} which uses an id to load its {@link ProcessNode}.
     * 
     * If {@link ProcessNode}s were {@link Serializable} you could just use a standard {@link Model}.
     * 
     * @see #equals(Object)
     * @see #hashCode()
     */
    private static class ProcessNodeModel extends LoadableDetachableModel<ProcessNode> {
        private static final long serialVersionUID = 1L;
        private final ProjectResourceId id;

        public ProcessNodeModel(ProcessNode node) {
            super(node);
            id = node.getResourceId();
        }

        @Override
        protected ProcessNode load() {
            return modelManager.getProcessNode(id);
        }

        /**
         * Important! Models must be identifiable by their contained object.
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof ProcessNodeModel) {
                return ((ProcessNodeModel)obj).id.equals(id);
            }
            return false;
        }

        /**
         * Important! Models must be identifiable by their contained object.
         */
        @Override
        public int hashCode() {
            return id.hashCode();
        }
    }
}
