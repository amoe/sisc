package sisc.exprs;

import java.io.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.exprs.fp.OptimismUnwarrantedException;
import sisc.exprs.fp.OptimisticHost;
import sisc.exprs.fp.Utils;

public class IfEval extends Expression implements OptimisticHost {

    private static final int POS_CONSEQ=0, POS_ALTERN=1;

    public Expression conseq, altern;

    public IfEval(Expression conseq, Expression altern) {
        this.conseq=conseq;
        this.altern=altern;
    }

    public void setHosts() {
        Utils.linkOptimistic(this, conseq, POS_CONSEQ);
        Utils.linkOptimistic(this, altern, POS_ALTERN);
    }

    public void eval(Interpreter r) throws ContinuationException {
        boolean retry;
        do {
            try {
                r.next(SchemeBoolean.toBoolean(r.acc) ? conseq : altern);
                retry = false;
            } catch (OptimismUnwarrantedException uwe) {
                retry = true;
            }
        } while (retry);
    }

    public Value express() {
        return list(Symbol.get("if"), conseq.express(), altern.express());
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(conseq);
        s.writeExpression(altern);
    }

    public IfEval() {}

    public void deserialize(Deserializer s) throws IOException {
        conseq=s.readExpression();
        altern=s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(conseq) && v.visit(altern);
    }

    /* (non-Javadoc)
     * @see sisc.exprs.OptimisticHost#alter(int, sisc.data.Expression)
     */
    public synchronized void alter(Interpreter r, int uexpPosition, Expression replaceWith) {
        switch(uexpPosition) {
        case POS_CONSEQ:
            conseq = replaceWith;
            break;
        case POS_ALTERN:
            altern = replaceWith;
            break;
        }
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
