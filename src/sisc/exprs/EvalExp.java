package sisc.exprs;

import sisc.data.*;
import sisc.exprs.fp.OptimismUnwarrantedException;
import sisc.exprs.fp.OptimisticExpression;
import sisc.exprs.fp.OptimisticHost;
import sisc.exprs.fp.Utils;

import java.io.*;
import sisc.interpreter.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;

public class EvalExp extends Expression implements OptimisticHost {
    public static final int POS_PRE=0, POS_POST=1;
    
    public Expression pre, post;
    public boolean preImmediate;

    public EvalExp(Expression pre, Expression post, boolean preImmediate) {
        this.pre=pre;
        this.post=post;
        this.preImmediate=preImmediate;
    }

    public void setHosts() {
        /*
          'post' must not be an OptimisticExpression if 'pre' is
          non-immediate or an OptimisticExpression. It could end up on
          the stack and hence be evaluated outside the context of the
          eval method here.
        */
        if (!preImmediate || pre instanceof OptimisticExpression) {
            Utils.assertNonOptimistic(post);
        }

        Utils.linkOptimistic(this, pre, POS_PRE);
        Utils.linkOptimistic(this, post, POS_POST);
    }

    public void eval(Interpreter r) throws ContinuationException {
        boolean retry;
        do {
            try {
                if (preImmediate) {
                    r.acc = pre.getValue(r);
                    r.next(post);
                } else {
                    r.push(post);
                    r.next(pre);
                }      
                retry = false;
            } catch (OptimismUnwarrantedException uwe) {
                retry = true;
            }
        } while (retry);
    }

    private Value expressHelper() {
        return new Pair(pre.express(),
                        ((post instanceof EvalExp) ?
                         ((EvalExp)post).expressHelper() :
                         list(post.express())));
    }

    public Value express() {
        return new Pair(sym("begin"), expressHelper());
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(pre);
        s.writeExpression(post);
        s.writeBoolean(preImmediate);
    }

    public EvalExp() {}

    public void deserialize(Deserializer s) throws IOException {
        pre=s.readExpression();
        post=s.readExpression();
        preImmediate=s.readBoolean();
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(pre) && v.visit(post);
    }

    /* (non-Javadoc)
     * @see sisc.exprs.OptimisticHost#alter(int, sisc.data.Expression)
     */
    public synchronized void alter(Interpreter r, int uexpPosition, Expression replaceWith) {
        switch(uexpPosition) {
        case POS_PRE:
            pre=replaceWith;
            if (preImmediate && !(replaceWith instanceof Immediate)) {
                preImmediate=false;
            }
            break;
        case POS_POST:
            post=replaceWith;
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
