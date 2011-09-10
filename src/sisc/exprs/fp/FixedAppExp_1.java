package sisc.exprs.fp;

import java.io.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.FixableProcedure;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.util.FreeReference;

public class FixedAppExp_1 extends FixedAppExp_0 implements OptimisticHost {
    public Immediate op0;

    public FixedAppExp_1(Immediate op0, 
                   FreeReference ref) {
        super(ref);
        this.op0=op0;
    }

    public void setHosts() {
        Utils.linkOptimistic(this, (Expression)op0, 0);
    }
    
    public Expression[] getOperands() {
        return new Expression[] {(Expression)op0};
    }

    /* (non-Javadoc)
     * @see sisc.exprs.fp.OptimisticHost#alter(int, sisc.data.Expression)
     */
    public void alter(Interpreter r, int uexpPosition, Expression replaceWith) {
        Expression[] rands = getOperands();
        rands[uexpPosition] = replaceWith;
        revert(r, rands);
    }

    public Value doGetValue(FixableProcedure proc, Interpreter r) throws ContinuationException {
        return proc.apply(op0.getValue(r));
    }

    public Value express() {
        return list(Symbol.get("fapp"), ref.express(),
                    ((Expression)op0).express());
    }

    public void serialize(Serializer s) throws IOException {
        super.serialize(s);
        s.writeExpression((Expression)op0);
    }

    public FixedAppExp_1() {}

    public void deserialize(Deserializer s) throws IOException {
        super.deserialize(s);
        op0=(Immediate)s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        return super.visit(v) && v.visit((Expression)op0);
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
