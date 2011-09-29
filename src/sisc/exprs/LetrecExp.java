package sisc.exprs;

import sisc.data.*;
import sisc.env.LexicalUtils;
import sisc.interpreter.*;
import sisc.ser.*;
import java.io.IOException;

public class LetrecExp extends AppExp {

    int lcount, localIndices[], lexicalIndices[];

    public LetrecExp(Expression exp, Expression rands[], Expression nxp, 
                     int[] localIndices, int[] lexicalIndices,
                     boolean allImmediate) {
        super(exp, rands, nxp, allImmediate);
        this.localIndices=localIndices;
        this.lexicalIndices=lexicalIndices;
        lcount=localIndices.length+lexicalIndices.length;
    }

    public void eval(Interpreter r) throws ContinuationException {
        r.env=LexicalUtils.fixLexicals(r, lcount, localIndices, lexicalIndices);
        r.lcl=r.createValues(rands.length);
        for (int i=rands.length-1; i>=0; i--)
            r.lcl[i]=new Box(VOID);
        super.eval(r);
    }

    public Value express() {
        Pair lccps=LexicalUtils.intArrayToList(localIndices);
        Pair lxcps=LexicalUtils.intArrayToList(lexicalIndices);
        Pair args=EMPTYLIST;
        for (int i=rands.length-1; i>=0; i--) {
            args=new Pair(((rands[i]==null) ? VOID : rands[i].express()), args);
        }
        return list(Symbol.get("letrec"),
                    list(lccps, lxcps),
                    args,
                    exp.express(),
                    nxp.express());
    }

    public LetrecExp() {}
    public void serialize(Serializer s) throws IOException {
        super.serialize(s);
        LexicalUtils.writeIntArray(s,localIndices);
        LexicalUtils.writeIntArray(s,lexicalIndices);
    }

    public void deserialize(Deserializer s) throws IOException {
        super.deserialize(s);
        localIndices=LexicalUtils.readIntArray(s);
        lexicalIndices=LexicalUtils.readIntArray(s);
        lcount=((localIndices==null ? 0 : localIndices.length) +
                (lexicalIndices==null ? 0 : lexicalIndices.length));
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
