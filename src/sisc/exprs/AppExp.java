package sisc.exprs;

import java.io.*;
import sisc.data.*;
import sisc.exprs.fp.OptimismUnwarrantedException;
import sisc.exprs.fp.OptimisticExpression;
import sisc.exprs.fp.OptimisticHost;
import sisc.exprs.fp.Utils;
import sisc.interpreter.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.compiler.Compiler;

public class AppExp extends Expression implements OptimisticHost {
    public static final int POS_EXP=-1, POS_NXP=-2;
 
    public Expression exp, rands[], nxp;
    public boolean allImmediate;
    protected int l;

    public AppExp(Expression exp, Expression rands[], Expression nxp, 
                  boolean allImmediate) {
        this.exp=exp;
        this.rands=rands;
        l=rands.length;
        this.nxp = nxp;
        this.allImmediate=allImmediate;
    }

    public void setHosts() {
        /*
          'nxp' must not be an OptimisticExpression if we have
          non-immediate or OptimisticExpression operands. It could end
          up on the stack and hence be evaluated outside the context
          of the eval method here.
        */
        if (!allImmediate || haveOptimisticRands()) {
            Utils.assertNonOptimistic(nxp);
        }

        Utils.linkOptimistic(this, exp, POS_EXP);
        Utils.linkOptimistic(this, nxp, POS_NXP);
        for (int i=0; i<rands.length; i++) {
            Utils.linkOptimistic(this, rands[i], i);
        }
    }

    private boolean haveOptimisticRands() {
        for (int i=0; i<rands.length; i++) {
            if (rands[i] instanceof OptimisticExpression) return true;
        }

        return false;
    }

    public void eval(Interpreter r) throws ContinuationException {

        r.newVLR(l);

        boolean retry;
        do {
            try {
                if (allImmediate) {
                    r.acc=exp.getValue(r);
                    // Load the immediates from right to left
                    for (int i = l-1; i>=0; i--) {
                        r.vlr[i] = rands[i].getValue(r);
                    }
                    r.next(nxp);
                } else {
                    // Load the immediates from right to left
                    Expression ex;
                    for (int i = l-1; i>=0; i--) {
                        ex=rands[i];
                        if (ex != null)
                            r.vlr[i] = ex.getValue(r);
                    }
                    r.push(nxp);
                    r.next(exp);
                }         
                retry = false;
            } catch (OptimismUnwarrantedException uwe) {
                retry = true;
            }
        } while (retry);
    }

    public Value express() {
        Pair args=EMPTYLIST;
        for (int i=rands.length-1; i>=0; i--) {
            args=new Pair(((rands[i]==null) ? VOID : rands[i].express()), args);
        }
        return list(Symbol.get("app"),
                    args,
                    exp.express(),
                    nxp.express());
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(exp);
        s.writeInt(l);
        for (int i=0; i<l; i++) {
            s.writeExpression(rands[i]);
        }
        s.writeExpression(nxp);
        s.writeBoolean(allImmediate);
    }

    public AppExp() {}

    public void deserialize(Deserializer s) throws IOException {
        exp=s.readExpression();
        l=s.readInt();
        rands=new Expression[l];
        for (int i=0; i<l; i++) {
            rands[i]=s.readExpression();
        }
        nxp=s.readExpression();
        allImmediate=s.readBoolean();
    }

    public boolean visit(ExpressionVisitor v) {
        if (!v.visit(exp)) return false;
        for (int i=0; i<rands.length; i++) {
            if (!v.visit(rands[i])) return false;
        }
        return v.visit(nxp);
    }

    /* (non-Javadoc)
     * @see sisc.exprs.OptimisticHost#alter(int, sisc.data.Expression)
     */
    public synchronized void alter(Interpreter r, int uexpPosition, Expression replaceWith) {
        switch(uexpPosition) {
        case POS_EXP:
            exp=replaceWith;
            break;
        case POS_NXP:
            nxp=replaceWith;
            break;
        default:
            if (replaceWith instanceof Immediate) {
                rands[uexpPosition] = replaceWith;
            } else {
                nxp = Compiler.makeFillRib(r, exp, replaceWith, uexpPosition, nxp, false);
                exp = replaceWith;
                rands[uexpPosition] = null;
                ((FillRibExp)nxp).setHosts();
            }
        }

        if (allImmediate && !(replaceWith instanceof Immediate)) {
            allImmediate=false;
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
