package com.ils.common.component.recmap.delegate;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import org.python.core.CompileMode;
import org.python.core.CompilerFlags;
import org.python.core.Py;
import org.python.core.PyCode;
import org.python.core.PyStringMap;

import com.ils.common.component.recmap.RecommendationMap;
import com.inductiveautomation.ignition.common.script.JythonExecException;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * For use as a JMenuItem action. In particular we need to pass the
 * recmap, script manager and row. The scripts all have the same 
 * signature.
 * @author chuckc
 *
 */
public class ScriptAction extends AbstractAction {
	private static final long serialVersionUID = -252635731301216355L;
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private static final String MODULE = "ils.diagToolkit.recommendationMap";
	private final RecommendationMap map;
	private final String method;
	private final int row;

	public ScriptAction(RecommendationMap recmap,String text,String method,int row) {
		super(text);
		this.map = recmap;
		this.method = method;
		this.row = row;
	}

	// The action is to run the script. We do everything on-demand.
	public void actionPerformed(ActionEvent actionEvent) {
		Runnable runnable = new Runnable() {
			public void run() {
				ScriptManager mgr = map.getScriptManager();
				try {
					PyStringMap localMap = mgr.createLocalsMap();
					localMap.__setitem__("theMap",Py.java2py(map));
					localMap.__setitem__("index",Py.java2py(row));
					String script = buildScript(MODULE,method);
					PyCode code = compile(script);
					mgr.runCode(code, localMap);
				}
				catch(JythonExecException jee) {
					log.warnf("ScriptAction.actionPerformed: Exeception executing %s (%s)",method,jee.getLocalizedMessage());
				}
			}
		};
		EventQueue.invokeLater(runnable);
	}
	
	// ==================================== Helper Methods ===========================
	// Arglist is fixed
	private String buildScript(String moduleName,String entry) {
		StringBuffer sb = new StringBuffer();
		sb.append(String.format("import %s;",moduleName));
		sb.append(String.format("%s.%s(theMap,index);",moduleName,entry));
		return sb.toString();
	}
	
	private PyCode compile(String script) {
		PyCode code = Py.compile_flags(script,"ils",CompileMode.exec,CompilerFlags.getCompilerFlags());
		return code;
	}
}

