package sisc.exprs;

import java.io.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.env.SymbolicEnvironment;
import sisc.util.ExpressionVisitor;
import sisc.util.FreeReference;

public class DefineEval extends Expression {

    private FreeReference ref;

    public DefineEval(Symbol sym, SymbolicEnvironment senv) {
        ref = new FreeReference(sym, senv);
    }

    public void eval(Interpreter r) throws ContinuationException {
        ref.define(r.acc);
        r.acc=VOID;
        r.nxp=null;
    }

    public Value express() {
        return new Pair(sym("define"), ref.express());
    }

    public void serialize(Serializer s) throws IOException {
        ref.serialize(s);
    }

    public void deserialize(Deserializer s) throws IOException {
        ref.deserialize(s);
    }

    public DefineEval() {
        ref = new FreeReference();
    }

    public boolean equals(Object o) {
        if (!(o instanceof DefineEval))
            return false;
        return ref.equals(((DefineEval)o).ref);
    }

    public int hashCode() {
        return ref.hashCode();
    }

    public boolean visit(ExpressionVisitor v) {
        return ref.visit(v);
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
