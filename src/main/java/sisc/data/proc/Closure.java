package sisc.data.proc;

import java.io.*;
import sisc.interpreter.*;
import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.data.*;
import sisc.env.LexicalUtils;

public class Closure extends Procedure implements NamedValue {
    public boolean arity;
    public int fcount, boxes[];
    private transient int bl;
    public Value[] env;
    public Expression body;

    public Closure(boolean arity, int fcount, Expression body,
                   Value[] env, int[] boxes) {
        this.arity=arity;
        this.fcount=fcount;
        this.env=env;
        this.body=body;
        this.boxes=boxes;
        bl=(boxes == null ? -1 : boxes.length-1);
    }

    private final Value[] matchArgs(Interpreter r)
        throws ContinuationException {
      
        Value[] vals;
        
        if (!arity) {
            if (bl < 0) {
                // No boxing will occur

                /**
                 * It is safe to use the vlr directly as our arg array,
                 * without copying. That is because any future or concurrent
                 * modification of the vlr is impossible since (a) this code is
                 * the *application* of a function, so all writing to vlr in
                 * order to supply the arguments will have been done, (b) k
                 * invocations copy the vlr before modification, and 
                 * (c) this path will not side-effect the vlr, which in rare cases
                 * (such as ((call/cc call/cc) (lambda (f) f))) may
                 * corrupt another saved vlr.
                 **/
                vals=r.vlr;
            } else {
                /**
                 * Because we're boxing, we *will* side-effect the vlr, violating (c) 
                 * from the above description, so we must copy if the vlr is locked,
                 * otherwise the Box may leak into another continuation's, which 
                 * may be unexpected and crash the LexicalReferenceExp, or at least 
                 * allow set!'s on the box to propogate incorrectly to another context.
                 */
                vals=r.vlrToArgs();
            }
            
            if (vals.length != fcount) {
                error(r, liMessage(SISCB,"notenoughargsto", toString(),
                                   fcount, r.vlr.length));
                return null;
            }
        } else {
            
            if (r.vlr.length < (fcount-1)) {
                error(r, liMessage(SISCB,"notenoughargstoinf", toString(),
                                   fcount-1, r.vlr.length));
                return null;
            }
            
            /**
             * We must copy if the vlr is locked here, because the act of
             * setting the last vlr slot to the prepared list of rest
             * arguments is a side-effect, and can propogate to other
             * captures of that vlr in continuations.
             */
            vals=r.vlrToRestArgs(fcount);
        }

        for (int i=bl; i>=0; i--) {
            int bi=boxes[i];
            vals[bi]=new Box(vals[bi]);
        }
        return vals;
    }

    public void apply(Interpreter r) throws ContinuationException {
        r.lcl=matchArgs(r);
        
        // The VLR register must be cleared here, since its no longer needed,
        // otherwise it may be pushed onto the stack as garbage
        // which can result in problems with for example serialization.
        //
        // Note that its *not* returned using r.returnVLR(), because
        // we aren't saying that the VLR array itself is recyclable, just
        // that in this evaluation subtree it shouldn't be visible.
        r.vlr=null;
        r.env=env;
        r.nxp=body;
    }
    
    public void display(ValueWriter w) throws IOException {
        displayNamedOpaque(w, "procedure");
    }

    public void serialize(Serializer s) throws IOException {
        long attr=(long)fcount << 1;
        if (arity) attr|=1;
        s.writeLong(attr);
        LexicalUtils.writeIntArray(s, boxes);
        s.writeExpressionArray(env);
        s.writeExpression(body);
    }

    public Value express() {
        Pair boxs = LexicalUtils.intArrayToList(boxes);
        return list(Symbol.get("closure"),
                    new Pair(SchemeBoolean.get(arity),
                             new Pair(Quantity.valueOf(fcount),
                                      boxs)),
                    valArrayToVec(env),
                    body.express());
    }

    public Closure() {}

    public void deserialize(Deserializer s) throws IOException {
        long attr=s.readLong();
        fcount=(int)(attr>>1);
        arity=(attr&1)!=0;
        boxes=LexicalUtils.readIntArray(s);
        bl=(boxes == null ? -1 : boxes.length-1);
        env=s.readValueArray();
        body=s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        return super.visit(v) && CallFrame.visitValueArray(v, env) && 
            v.visit(body);
    }

    /*
    //Profiling
    long ec=0;

    static {
        System.runFinalizersOnExit(true);
    }

    protected void finalize() {
        if (name!=null) {
            System.err.println("C"+justify(""+ec, 10, ' ')+" "+name);
        }
    }           
    */
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
