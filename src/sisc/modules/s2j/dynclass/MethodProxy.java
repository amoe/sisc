/**
 * 
 */
package sisc.modules.s2j.dynclass;

import sisc.data.Procedure;
import sisc.data.Value;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeCaller;
import sisc.interpreter.SchemeException;
import sisc.modules.s2j.JavaObject;
import sisc.modules.s2j.Util;

public abstract class MethodProxy {
    
    public static final Object unwrap(Object object) {
        if (object instanceof JavaObject) {
            return ((JavaObject)object).get();
        } else return object;
    }
    
    public static Object invoke(SchemeHook proc, Object thisObj, Object[] args) throws Throwable {
        Interpreter r=Context.enter(proc.env);
        try {
            Value[] vargs=r.createValues(args.length+1);
            vargs[0]=Util.makeJObj(thisObj);
            for (int i=vargs.length-1; i>=0; i--) {
                vargs[i+1]=Util.makeJObj(args[i]);
            }
            Object result=r.eval(proc.proc, vargs);
            return unwrap(result);
        } catch (SchemeException e) {
            throw Util.javaException(e);
        } finally {
            Context.exit();
        }
    }
}