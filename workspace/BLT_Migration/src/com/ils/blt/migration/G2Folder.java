package com.ils.blt.migration;



/**
 * A G2Folder represents the sub-workspace of a button.
 * The name of the folder is the name of the button.
 */
public class G2Folder {
	private String name; 
	private String comments;

	private G2Folder[] folders;
	private G2Diagram[] problems;
	
	public G2Folder() {	
		folders = new G2Folder[0];
		problems = new G2Diagram[0];
		name="UNSET";
		comments = "";
	}
	
	public String getComments() {return comments;}
	public G2Diagram[] getProblems() { return problems; }
	public String getName() { return name; }
	public G2Folder[] getFolders() { return folders; }
	public void setComments(String comments) {this.comments = comments;}
	public void setProblems(G2Diagram[] list) { problems=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setFolders(G2Folder[] array) { this.folders = array; }

}
