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

package com.ils.blt.gateway.wicket.content;

import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.tree.AbstractTree;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

import com.ils.blt.gateway.engine.ProcessNode;

public abstract class Content implements IDetachable {
	private static final long serialVersionUID = 550568434948280302L;

	/**
     * Create new content.
     */
    public abstract Component newContentComponent(String id, AbstractTree<ProcessNode> tree,
        IModel<ProcessNode> model);

    @Override
    public void detach()  {
    }
}