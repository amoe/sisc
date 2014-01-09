package sisc.interpreter;

import java.io.PrintStream;
import java.io.PrintWriter;
import sisc.util.Util;
import sisc.data.Procedure;
import sisc.data.Value;
import sisc.data.Symbol;
import sisc.data.Pair;
import sisc.data.SchemeString;
import sisc.interpreter.Context;
import sisc.interpreter.SchemeCaller;
import sisc.interpreter.Interpreter;

public class SchemeException extends Exception {
    public Pair m;
    public Procedure e, f;

    public SchemeException(Pair message, Procedure exception_k, 
                           Procedure parent_fk) {
        m=message;
        e=exception_k;
        f=parent_fk;
    }

    /**
     * @return the Scheme message Pair as String
     */
    public String getMessage() {
        return m.toString();
    }

    /**
     * @return the bare Scheme message text 
     */
    public String getMessageText() {
        return ((Pair)m.car()).cdr().toString();
    }

    public String schemeStackTrace() {
        try {
            SchemeString res =
                (SchemeString)Context.execute(new SchemeCaller() {
                        public Object execute(Interpreter r)
                            throws SchemeException {
                            Procedure converter =
                                (Procedure)r.lookup(Symbol.get("error->string"),
                                                  Util.TOPLEVEL);
                            return r.eval(converter, new Value[]{m,e});
                        }
                    });
            return "Scheme exception:\n" + res.asString();
        } catch (Exception e) {
            return "";
        }
    }

    public void printStackTrace(PrintStream s) {
        super.printStackTrace(s);
        s.print(schemeStackTrace());
    }

    public void printStackTrace(PrintWriter s) {
        super.printStackTrace(s);
        s.print(schemeStackTrace());
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
