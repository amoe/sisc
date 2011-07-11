package sisc.modules.s2j;

import java.lang.reflect.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.env.DynamicEnvironment;

public class SchemeInvocation implements InvocationHandler {
    protected DynamicEnvironment dynenv;
    protected Procedure proc;

    public SchemeInvocation(DynamicEnvironment dynenv, Procedure proc) {
        this.dynenv = dynenv;
        this.proc = proc;
    }

    public Object invoke(Object proxy, Method m, Object[] args)
        throws Throwable {
        Interpreter r = Context.currentInterpreter(dynenv.ctx);
        r = Context.enter((r == null) ? dynenv.copy() : r.dynenv);
        Pair p = Util.EMPTYLIST;
        if (args != null) { //for some reason args can be null
            Class[] pTypes = m.getParameterTypes();
            for (int i=args.length-1; i>=0; i--) {
                p = new Pair(Util.makeJObj(args[i], pTypes[i]), p);
            }
        }
        Value res = null;
        try {
            res = r.eval(proc, new Value[] {Util.makeJObj(proxy, Object.class),
                                            Util.makeJObj(m, Method.class),
                                            p});
        } catch (SchemeException e) {
            throw Util.javaException(e);
        } finally {
            Context.exit();
        }
        return (m.getReturnType().isAssignableFrom(Void.TYPE) ? null : Util.jobj(res));
    }
}

/*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 * 
 * The Original Code is the Second Interpreter of Scheme Code (SISC).
 * 
 * The Initial Developer of the Original Code is Scott G. Miller.
 * Portions created by Scott G. Miller are Copyright (C) 2000-2007
 * Scott G. Miller.  All Rights Reserved.
 * 
 * Contributor(s):
 * Matthias Radestock 
 * 
 * Alternatively, the contents of this file may be used under the
 * terms of the GNU General Public License Version 2 or later (the
 * "GPL"), in which case the provisions of the GPL are applicable 
 * instead of those above.  If you wish to allow use of your 
 * version of this file only under the terms of the GPL and not to
 * allow others to use your version of this file under the MPL,
 * indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by
 * the GPL.  If you do not delete the provisions above, a recipient
 * may use your version of this file under either the MPL or the
 * GPL.
 */
