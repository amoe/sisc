package sisc.modules.s2j.dynclass;

import sisc.data.Procedure;
import sisc.env.DynamicEnvironment;

/**
 * A SchemeHook is the combination of the dynamic environment, and a procedure,
 * so that at call time the procedure is called in the correct environment.
 * This allows multiple SISC AppContexts and dynamic environments to be called
 * from dynamic classes which are scattered throughout a Java application and
 * who have no concept that the class is underpined in Scheme.
 *
 */
public class SchemeHook {

    DynamicEnvironment env;
    Procedure proc;
    
    public SchemeHook(DynamicEnvironment env, Procedure proc) {
        this.env = env;
        this.proc = proc;
    }
}
