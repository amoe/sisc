package sisc.data.proc;

import java.io.*;
import sisc.interpreter.*;
import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.data.Box;
import sisc.data.Expression;
import sisc.data.NamedValue;
import sisc.data.Pair;
import sisc.data.Procedure;
import sisc.data.Quantity;
import sisc.data.Value;
import sisc.env.LexicalUtils;

/**
 * A closure which doesn't need to do any boxing and is of fixed arity.
 * @author smiller
 *
 */
public class SimpleClosure extends Procedure implements NamedValue {
    public int fcount;
    public Value[] env;
    public Expression body;

    public SimpleClosure(int fcount, Expression body, Value[] env) {
        this.fcount=fcount;
        this.env=env;
        this.body=body;
    }
    
    public void apply(Interpreter r) throws ContinuationException {
    	if (r.vlr.length != fcount) {
        	error(r, liMessage(SISCB,"notenoughargsto", toString(),
        			fcount, r.vlr.length));
        } 
        r.lcl=r.vlr;
        
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
    	s.writeInt(fcount);
        s.writeExpressionArray(env);
        s.writeExpression(body);
    }

    public Value express() {
        return list(sym("closure"),
        			list(Quantity.valueOf(fcount)),
                    valArrayToVec(env),
                    body.express());
    }

    public SimpleClosure() {}

    public void deserialize(Deserializer s) throws IOException {
    	fcount=s.readInt();
        env=s.readValueArray();
        body=s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        return super.visit(v) && CallFrame.visitValueArray(v, env) && 
            v.visit(body);
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
