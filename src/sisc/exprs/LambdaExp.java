package sisc.exprs;

import java.io.*;
import sisc.data.*;
import sisc.data.proc.Closure;
import sisc.data.proc.SimpleClosure;
import sisc.env.LexicalUtils;
import sisc.interpreter.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;

public class LambdaExp extends Expression implements Immediate {
	public transient boolean simple;
    public boolean infiniteArity;
    public int fcount, lcount, localIndices[], lexicalIndices[], boxes[];
    public Expression body;

    public LambdaExp(int s, Expression body, boolean arity,
                     int[] localids, int[] lexids, int[] boxes) {
    	if (!arity && boxes==null) {
    		simple=true;
    	} else {
    		simple=false;
            infiniteArity=arity;
            this.boxes=boxes;
    	}
        fcount=s;
        this.body=body;
        localIndices=localids;
        lexicalIndices=lexids;
        lcount=localids.length+lexids.length;
    }

    public void eval(Interpreter r) throws ContinuationException {
        r.acc=getValue(r);
        r.nxp=null;
    }
    
    public Value getValue(Interpreter r) throws ContinuationException {
    	if (simple) {
    		return new SimpleClosure(fcount,body,
    				                 LexicalUtils.fixLexicals(r, lcount, localIndices, lexicalIndices));
    	} else {
    		return new Closure(infiniteArity, fcount, body, 
    				LexicalUtils.fixLexicals(r, lcount, localIndices, lexicalIndices), 
    				boxes);
    	}
    }

    public Value express() {
        Pair lccps = LexicalUtils.intArrayToList(localIndices);
        Pair lxcps = LexicalUtils.intArrayToList(lexicalIndices);
        Pair boxs  = LexicalUtils.intArrayToList(boxes);
        return list(sym("lambda"),
                    new Pair(truth(infiniteArity),
                             new Pair(Quantity.valueOf(fcount),
                                      boxs)),
                    list(lccps, lxcps),
                    body.express());
    }

    public void serialize(Serializer s) throws IOException {
        s.writeBoolean(infiniteArity);
        s.writeInt(fcount);
        LexicalUtils.writeIntArray(s,localIndices);
        LexicalUtils.writeIntArray(s,lexicalIndices);
        LexicalUtils.writeIntArray(s,boxes);
        s.writeExpression(body);
    }

    public LambdaExp() {}

    public void deserialize(Deserializer s) throws IOException {
        infiniteArity=s.readBoolean();
        fcount=s.readInt();
        localIndices=LexicalUtils.readIntArray(s);
        lexicalIndices=LexicalUtils.readIntArray(s);
        lcount=((localIndices==null ? 0 : localIndices.length) +
                (lexicalIndices==null ? 0 : lexicalIndices.length));
        boxes=LexicalUtils.readIntArray(s);
        body=s.readExpression();
        simple=!infiniteArity && boxes==null;
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(body);
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
