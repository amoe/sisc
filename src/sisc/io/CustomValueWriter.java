package sisc.io;

import java.io.IOException;

import sisc.data.Pair;
import sisc.data.Procedure;
import sisc.data.Value;
import sisc.env.DynamicEnvironment;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;
import sisc.modules.Types.SchemeType;
import sisc.modules.io.IO;
import sisc.modules.s2j.JavaObject;
import sisc.util.Util;

public class CustomValueWriter implements ValueWriter {

	ValueWriter parent;
	private DynamicEnvironment dynenv;
	
	public CustomValueWriter(ValueWriter parent, DynamicEnvironment env) {
		this.parent=parent;
		this.dynenv=env;
	}
	
	protected Procedure resolvePrinter(Pair typeMap, Value v) {
		while (typeMap != Util.EMPTYLIST) {
			Pair aentry=Util.pair(typeMap.car());
			Value type=aentry.car();
			Class typeClass=null;
			if (type instanceof JavaObject) {
				typeClass=(Class)((JavaObject)type).get();
			} else if (type instanceof SchemeType) {
				typeClass=((SchemeType)type).getClassObject();
			}
			if (typeClass != null && v.getClass().isAssignableFrom(typeClass)) {
				return Util.proc(aentry.cdr());
			}
			typeMap=(Pair)typeMap.cdr();
		}
		return null;
	}

	
	protected String customPrint(Procedure proc, Value v) {
        Interpreter r=Context.enter();
        try {
            return Util.string(r.eval(proc, new Value[] {v}));
        } catch (SchemeException e) {
        	Procedure.throwNestedPrimException(Util.liMessage(IO.IOB, "customporterror", e.getMessageText()), e);
        	return null;
        } finally {
            Context.exit();
        }
	}


	public void display(Value v) throws IOException {
		Procedure proc=resolvePrinter(dynenv.customDisplayTypeMap, v);
		if (proc == null) parent.display(v);
		else {
			parent.append(customPrint(proc, v));
		}
	}

	public void write(Value v) throws IOException {
		Procedure proc=resolvePrinter(dynenv.customWriteTypeMap, v);
		if (proc == null) parent.write(v);
		else {
			parent.append(customPrint(proc, v));
		}
	}

	public ValueWriter append(Value v) throws IOException {
		return parent.append(v);
	}

	public ValueWriter append(char c) throws IOException {
		return parent.append(c);
	}

	public ValueWriter append(String s) throws IOException {
		return parent.append(s);
	}

	public boolean isInlinable(Value v) {
		return parent.isInlinable(v);
	}

	public boolean vectorLengthPrefixing() {
		return parent.vectorLengthPrefixing();
	}

	public boolean caseSensitive() {
		return parent.caseSensitive();
	}

}
