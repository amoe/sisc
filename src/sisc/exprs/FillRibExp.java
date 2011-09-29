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

public class FillRibExp extends Expression implements OptimisticHost {
    static final int POS_EXP=0, POS_NXP=1;

    public Expression exp, nxp;
    public int pos;
    public boolean lastAndRatorImmediate;

    public FillRibExp(Expression exp, int pos, Expression nxp, boolean lari) {
        this.exp=exp;
        this.pos=pos;
        this.nxp=nxp;
        lastAndRatorImmediate=lari;
    }

    public void setHosts() {
        /*
          'nxp' must not be an OptimisticExpression if it can end up
          on the stack and hence be evaluated outside the context of
          the eval method here.
        */
        if (!lastAndRatorImmediate || exp instanceof OptimisticExpression) {
            Utils.assertNonOptimistic(nxp);
        }
        Utils.linkOptimistic(this, exp, POS_EXP);
        Utils.linkOptimistic(this, nxp, POS_NXP);
    }
    
    public void eval(Interpreter r) throws ContinuationException {

        r.setVLR(pos, r.acc);

        boolean retry;
        do {
            try {
                if (lastAndRatorImmediate) {
                    r.acc=exp.getValue(r);
                    r.next(nxp);
                } else {
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
        return list(Symbol.get("arg"),
                    Quantity.valueOf(pos),
                    exp.express(),
                    nxp.express());
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(exp);
        s.writeInt(pos);
        s.writeExpression(nxp);
        s.writeBoolean(lastAndRatorImmediate);
    }

    public FillRibExp() {}

    public void deserialize(Deserializer s) throws IOException {
        exp=s.readExpression();
        pos=s.readInt();
        nxp=s.readExpression();
        lastAndRatorImmediate=s.readBoolean();
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(exp) &&  v.visit(nxp);
    }

    public synchronized void alter(Interpreter r, int uexpPosition, Expression replaceWith) {
        switch(uexpPosition) {
        case POS_NXP:
            nxp=replaceWith;
            break;
        case POS_EXP:
            exp=replaceWith;
            if (lastAndRatorImmediate && !(replaceWith instanceof Immediate)) {
                lastAndRatorImmediate=false;
            }
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
