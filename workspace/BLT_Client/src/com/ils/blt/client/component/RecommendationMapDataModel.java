/**
 * Copyright 2014. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.zip.GZIPInputStream;

import prefuse.data.Graph;
import prefuse.data.Table;
import prefuse.data.Tree;

import com.inductiveautomation.factorypmi.application.binding.VisionClientContext;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.sfc.SFCModule;
import com.inductiveautomation.sfc.api.XMLParseException;
import com.inductiveautomation.sfc.client.api.ClientStepRegistry;
import com.inductiveautomation.sfc.client.api.ClientStepRegistryProvider;
import com.inductiveautomation.sfc.definitions.ChartDefinition;
import com.inductiveautomation.sfc.definitions.ElementDefinition;
import com.inductiveautomation.sfc.definitions.StepDefinition;
import com.inductiveautomation.sfc.elements.steps.enclosing.EnclosingStepProperties;
import com.inductiveautomation.sfc.uimodel.ChartCompilationResults;
import com.inductiveautomation.sfc.uimodel.ChartCompiler;
import com.inductiveautomation.sfc.uimodel.ChartUIModel;

/** 
 * This class holds SfcChart objects in a Tree structure. The tree is derived from
 * serialized """ resources. We create the tree on startup and from then on rely on
 * project resource updates.
 */
public class RecommendationMapDataModel {
	private static final String TAG = "ChartTreeDataModel";
	public static final String CHART_RESOURCE_TYPE="sfc-chart-ui-model";
	public static final String FOLDER_RESOURCE_TYPE="__folder";
	private int ROOT_ROW = 0;           // Number of the root row
	private static final long STEP_RESOURCE = -2;       // Stand-in id for enclosing step
	// Table column names
	private static final String CXNS    = "Cxns";       // Incoming connection count
	public static final String KEY      = "Key";
	public static final String NAME     = "Name";
	public static final String PARENT   = "Parent";
	private static final String PATH    = "Path";       // Chart identifier
	public static final String RESOURCE = "Resource"; // ResourceId

	private final Map<String,Integer>  rowLookup;    // Find node row by path
	private final Map<String,FolderHolder> folderHierarchy;
	
	private final VisionClientContext context;
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final Table nodes;
	private final Table edges;
	
	public RecommendationMapDataModel(VisionClientContext ctx) {
		context = ctx;
		folderHierarchy = new HashMap<>();
		rowLookup = new HashMap<>();
		nodes = new Table();
		nodes.addColumn(CXNS, int.class);   // Count of linked connections
		nodes.addColumn(NAME, String.class);
		nodes.addColumn(KEY, int.class);            // Table row - key
		nodes.addColumn(PATH, String.class);
		nodes.addColumn(PARENT, String.class);
		nodes.addColumn(RESOURCE, long.class);
		
		edges = new Table();
		// The keys match the node key in the node table
		// The node direction is from parent to child.
		edges.addColumn(Graph.DEFAULT_SOURCE_KEY, int.class);
		edges.addColumn(Graph.DEFAULT_TARGET_KEY, int.class);
		
		initialize();
	}

	/**
	 * Initialize the UI
	 */
	private void initialize() {
		List<ProjectResource> resources = context.getGlobalProject().getProject().getResources();
		Object iaSfcHook = context.getModule(SFCModule.MODULE_ID);
		ClientStepRegistry registry = ((ClientStepRegistryProvider)iaSfcHook).getStepRegistry();
		// Initialize the folder hierachy
		UUID root = ChartUIModel.ROOT_FOLDER;
		log.tracef("%s.initialize: ROOT_FOLDER = %s",TAG,root.toString());
		configureRootNode();
		FolderHolder rootHolder = new FolderHolder(root,null,"");
		rootHolder.setPath("");
		folderHierarchy.put(root.toString(), rootHolder);
		
		for(ProjectResource res:resources) {
			if( res.getResourceType().equals(CHART_RESOURCE_TYPE)) {
				log.tracef("%s.initialize: found chart %s, parent = %s", TAG,res.getName(),res.getParentUuid().toString());

				try {
					GZIPInputStream xmlInput = new GZIPInputStream(new ByteArrayInputStream(res.getData()));
					ChartUIModel chartModel = ChartUIModel.fromXML(xmlInput,registry );
					ChartCompiler compiler = new ChartCompiler(chartModel,registry);
					ChartCompilationResults ccr = compiler.compile();
					if(ccr.isSuccessful()) {
						ChartDefinition definition = ccr.getChartDefinition();
						int row = addNodeTableRow(res.getName(),res.getResourceId());
						nodes.setString(row, PARENT, res.getParentUuid().toString());
						checkForEnclosingStep(row,definition.getBeginElement());
					}
					else {
						log.warnf("%s.initialize: Chart %s has compilation errors", TAG,res.getName());
					}
				}
				catch(IOException ioe ) {
					log.warnf("%s.initialize: IO Exception for %s (%s)", TAG,res.getName(),ioe.getLocalizedMessage());
				}
				catch(XMLParseException xpe ) {
					log.warnf("%s.initialize: Parse Exception for %s (%s)", TAG,res.getName(),xpe.getLocalizedMessage());
				}
			}
			// Handle the folder hierarchy
			else if( res.getResourceType().equals(FOLDER_RESOURCE_TYPE)) {
				UUID self = res.getDataAsUUID();
				FolderHolder holder = new FolderHolder(res.getDataAsUUID(),res.getParentUuid(),res.getName());
				folderHierarchy.put(self.toString(),holder);
				log.tracef("%s.initialize: folder resource %s (%s) (%s, parent %s)", TAG,res.getName(),res.getResourceType(),
						self.toString(),res.getParentUuid().toString());
				resolvePath(holder);  // High likelihood of success if we're traversing down the tree
			}
		}
		// Make table legal if it was empty
		if(nodes.getRowCount()==0)  {
			configureNodesAsEmpty();
		}
		else {
			// Resolve any folder paths
			resolveFolderHierarchy();
			resolveFolderPaths();
			connectLinkElements();
			resolveRootConnections();
			rowLookup.clear();   // Free memory
			folderHierarchy.clear();
		}
		log.tracef("%s.initialize ...COMPLETE", TAG);
	}
	
	/**
	 * @return a tree constructed out of the nodes and edges.
	 */
	public Tree getTree() {
		Tree tree = new Tree(nodes,edges,KEY,Graph.DEFAULT_SOURCE_KEY,Graph.DEFAULT_TARGET_KEY);
		return tree;
	}
	
	// Check and see if the referenced element is an enclosing step
	// @param parentRow the row in the nodes table corresponding to the enclosing block
	private void checkForEnclosingStep(int parentRow,ElementDefinition step) {
		StepDefinition stepDef = (StepDefinition)step;
		if( stepDef.getFactoryId().equals(EnclosingStepProperties.FACTORY_ID)) {
			String name = stepDef.getProperties().get(EnclosingStepProperties.Name);
			String path = stepDef.getProperties().get(EnclosingStepProperties.CHART_PATH);
			log.infof("%s.checkForEnclosingStep: enclosing step %s = %s", TAG,name,path);
				// Create the step-that-is-an-enclosure node
			int row = addNodeTableRow(name,STEP_RESOURCE);
			nodes.setInt(row, CXNS, 1);     // Incoming connection from parent chart
			nodes.setString(row, PATH, path);
			addEdgeTableRow(parentRow,row);
		}
				
		// Check descendents
		List<ElementDefinition> nextSteps = step.getNextElements();
		for( ElementDefinition ce:nextSteps) {
			checkForEnclosingStep(parentRow,ce);
		}
	}
	
	// Create a connection between nodes
	// @return the row corresponding to the newly created connection.
	private int addEdgeTableRow(int sourceRow,int destinationRow) {
		int row = edges.getRowCount();
		log.infof("%s.addEdgeTableRow: %d -> %d", TAG,sourceRow,destinationRow);
		edges.addRow();
		edges.setInt(row,Graph.DEFAULT_SOURCE_KEY,sourceRow);
		edges.setInt(row,Graph.DEFAULT_TARGET_KEY,destinationRow);
		return row;
	}
	// We've found a chart resource. Add it to the table
    // @param resourceId if negative, then this row corresponds to
	// @return the row corresponding to the newly discovered chart.
	private int addNodeTableRow(String name,long resourceId) {
		int row = nodes.getRowCount();
		log.infof("%s.addNodeTableRow: %d = %s", TAG,row,name);
		nodes.addRow();
		nodes.setInt(row,CXNS,0); 
		nodes.setString(row,NAME,name);
		nodes.setInt(row,KEY,row);
		//nodes.setString(row,ID,"Not-a-uuid");
		nodes.setLong(row,RESOURCE,resourceId);
		return row;
	}
	// Loop through all of the nodes-that-are-links and
	// create connections between the link nodes and the
	// charts that they point to. 
	private void connectLinkElements() {
		log.infof("%s.connectLinkElements ...", TAG);
		int maxRow = nodes.getRowCount();
		int row = 0;
		while(row<maxRow) {
			long rowResource = nodes.getLong(row, RESOURCE);
			if( rowResource==STEP_RESOURCE ) {
				String path = nodes.getString(row, PATH);
				if( path!=null ) {
					Integer targetRow = rowLookup.get(path);
					if( targetRow!=null) {
						long resourceId = nodes.getLong(targetRow.intValue(), RESOURCE);
						nodes.setLong(row, RESOURCE, resourceId);     // Set resource in the link row
						addEdgeTableRow(row,targetRow.intValue());    // Connect enclosing step to chart
						// Increment the connection count in the target row
						int connections = nodes.getInt(targetRow, CXNS);
						nodes.setInt(targetRow, CXNS,connections+1);
					}
					else {
						log.warnf("%s.connectLinkElements. No chart entry for path %s", TAG,path);
					}
				}
				else if( row!=ROOT_ROW ){
					log.warnf("%s.connectLinkElements. No path in node table for row %d", TAG,row);
				}
			}
			row++;
		}
	}
	// Configure the nodes table to display something reasonable
	// if it is otherwise empty.
	private void configureRootNode() {
		ROOT_ROW = nodes.getRowCount();
		nodes.addRow();
		log.infof("%s.configureRootNode. root", TAG);
		nodes.setString(ROOT_ROW,NAME,"root");
		nodes.setInt(ROOT_ROW,CXNS,0);
		nodes.setInt(ROOT_ROW,KEY,ROOT_ROW);
		nodes.setString(ROOT_ROW,PATH,"");
		nodes.setLong(ROOT_ROW,RESOURCE,-1);
	}
	// Configure the nodes table to display something reasonable
	// if it is otherwise empty.
	private void configureNodesAsEmpty() {
		int row = 0;
		nodes.addRow();
		log.warnf("%s.configureNodesAsEmpty. No charts", TAG);
		nodes.setString(row,NAME,"No charts");
		nodes.setInt(row,CXNS,0);
		nodes.setInt(row,KEY,0);
		nodes.setString(row,PATH,"/");
		nodes.setLong(row,RESOURCE,-1);
	}
	
	// Resolve folder hierarchy
	private void resolveFolderHierarchy() {
		log.infof("%s.resolveFolderHierarchy ...", TAG);
		int MAX_DEPTH = 100;
		int depth = 0;
		boolean success = false;
		while( !success && depth<MAX_DEPTH ) {
			success = true;
			for(FolderHolder holder:folderHierarchy.values()) {
				if( holder.getPath()==null) {
					if(!resolvePath(holder)) success = false;       // Didn't resolve
				}
			}
			depth++;
		}
		if(!success) log.warnf("%s.resolveFolderHierarchy. Failed to find paths for all folders", TAG);
	}
	
	// Loop through all of the nodes-that-are-charts and
	// set the path.
	private void resolveFolderPaths() {
		log.infof("%s.resolveFolderPaths ...", TAG);
		int maxRow = nodes.getRowCount();
		int row = 0;
		while(row<maxRow) {
			long resourceId = nodes.getLong(row, RESOURCE);
			if( resourceId>=0) {
				String parent = nodes.getString(row, PARENT);
				if( parent !=null ) {
					FolderHolder fh = folderHierarchy.get(parent);
					String path = fh.getPath();   // Parent path
					if( path!=null) {
						String name = nodes.getString(row, NAME);
						if( path.length()==0) path = name;
						else path = String.format("%s/%s",path,name);
						nodes.setString(row, PATH, path);
						log.infof("%s.resolveFolderPaths ... %d is %s", TAG,row,path);
						rowLookup.put(path,new Integer(row));   // So we can find this for links 
					}
					else {
						log.warnf("%s.resolveFolderPaths. No path for resource %d (parent=%s)", TAG,resourceId,parent);
					}
				}
				else {
					log.warnf("%s.resolveFolderPaths. Unknown parent for resource %d", TAG,resourceId);
				}
			}
			row++;
		}
	}
	
	private boolean resolvePath(FolderHolder holder) {
		boolean success = false;
		UUID parent = holder.getParent();
		FolderHolder parentHolder = folderHierarchy.get(parent.toString());
		if( parentHolder!=null ) {
			String path = parentHolder.getPath();
			if( path!=null) {
				if( path.length()==0) path = holder.getName();
				else path = String.format("%s/%s", path,holder.getName());
				holder.setPath(path);
				success = true;
			}
		}
		else {
			// We expect all to be resolved immediately, but are not assured of this.
			log.tracef("%s.resolvePath. Unresolved parent %s for folder %s", TAG,holder.getParent().toString(),holder.getId().toString());
		}
		return success;
	}

	// Loop through all of the nodes. If they haven't been connected to by anything,
	// then they should be connected to the root node.
	private void resolveRootConnections() {
		log.infof("%s.resolveRootConnections ...", TAG);
		int maxRow = nodes.getRowCount();
		int row = 0;
		while(row<maxRow) {
			int connections = nodes.getInt(row,CXNS);
			if( connections==0 && row!=ROOT_ROW) {
				addEdgeTableRow(ROOT_ROW,row);
			}
			row++;
		}
	}
	/**
	 * Helper class for use in trying to resolve folder paths
	 */
	private class FolderHolder {
		private final UUID id;
		private final UUID parentId;
		private final String name;
		private String path = null;   // As long as this is null, we're not resolved
		public FolderHolder(UUID uuid,UUID parentUUId,String folderName) {
			this.id = uuid;
			this.parentId = parentUUId;
			this.name = folderName;
		}
		public UUID getId() { return id; }
		public String getName() { return name;}
		public String getPath() { return path; }
		public UUID getParent() { return parentId; }
		public void setPath(String p) { this.path = p; }
	}
	
}
