package sisc.env;

import java.io.*;
import sisc.data.*;
import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;

public class LexicalEnvironment extends Value {
    public boolean locked;
    public LexicalEnvironment parent;
    public Value[] vals;

    public LexicalEnvironment() {
        this.vals=ZV;
    }

    public LexicalEnvironment(Value[] vals, LexicalEnvironment parent) {
        this.vals=vals;
        this.parent=parent;
    }

    public static final void lock(LexicalEnvironment env) {
        while (env != null && !env.locked) {
            env.locked=true;
            env=env.parent;
        } 
    }

    public final Value lookup(int depth, int pos) {
        LexicalEnvironment e = this;
        while(depth-- > 0) e = e.parent;
        return e.vals[pos];
    }

    public final void set(int depth, int pos, Value v) {
        LexicalEnvironment e = this;
        while(depth-- > 0)  e = e.parent;
        e.vals[pos]=v;
    }

    public void display(ValueWriter w) throws IOException {
        w.append("#<").append(liMessage(SISCB, "environment")).append('>');
    }

    public void serialize(Serializer s) throws IOException {
        s.writeInt(vals.length);
        for (int i=0; i<vals.length; i++)
            s.writeExpression(vals[i]);
        s.writeBoolean(locked);
        s.writeExpression(parent);
    }

    public void deserialize(Deserializer s) throws IOException {
        int size=s.readInt();
        vals=new Value[size];
        for (int i=0; i<size; i++)
            vals[i]=(Value)s.readExpression();
        locked=s.readBoolean();
        parent = (LexicalEnvironment)s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        for (int i=0; i<vals.length; i++) {
            if (!v.visit(vals[i])) return false;
        }
        return v.visit(parent);
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
