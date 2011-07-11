package sisc.data;

import java.io.*;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;

public class SchemeVector extends Value {
    public Value[] vals;
    protected int lastUnique=-1;

    public SchemeVector() {}

    public SchemeVector(int count) {
        this(count, Quantity.ZERO);
    }

    public SchemeVector(int count, Value initializer) {
        vals=new Value[count];
        for (int i=0; i<vals.length; i++) {
            vals[i]=initializer;
        }
    }

    public SchemeVector(Value[] v) {
        vals=v;
    }

    public boolean valueEqual(Value v) {
        if (v==this) return true;
        if (!(v instanceof SchemeVector)) return false;
        SchemeVector o=(SchemeVector)v;
        if (o.vals.length!=vals.length)
            return false;

        for (int i=0; i<vals.length; i++) {
            if (!vals[i].valueEqual(o.vals[i]))
                return false;
        }
        return true;
    }

    public int valueHashCode() {
        int res = 0;
        for (int i=0; i<vals.length; i++) {
            res ^= vals[i].valueHashCode();
        }
        return res;
    }

    public int findEnd() {
        if (lastUnique > -1) return lastUnique;
        if (vals.length>1) {
            Value v=vals[vals.length-1];
            for (int i=vals.length-2; i>=0; i--)
                if (!vals[i].eqv(v)) return lastUnique=i+2;
            return lastUnique=1;
        }
        return lastUnique=vals.length;
    }
    
    public void display(ValueWriter w) throws IOException {
        w.append('#');
        if (w.vectorLengthPrefixing())
            w.append(Integer.toString(vals.length));
        w.append('(');
        int l = w.vectorLengthPrefixing() ? findEnd() : vals.length;
        for (int i=0; i<l-1; i++) {
            w.append(vals[i]).append(' ');
        }
        if (l>0) w.append(vals[l-1]);
        w.append(')');
    }

    public void fill(Value v) {
        for (int i=0; i<vals.length; i++)
            vals[i]=v;
        lastUnique=-1;
    }

    public void set(int idx, Value v) {
        lastUnique=-1;
        vals[idx]=v;
    }

    public String synopsis(int length) {
        StringBuffer b=new StringBuffer();
        b.append('#').append(vals.length).append('(');
        int l=findEnd(), i=0;
        for (; i<l && b.length()<length; i++) {
            b.append(vals[i].synopsis(length-b.length()));
            if (i+1<l) b.append(' ');
        }
        if (i<l) b.append("...");
        else b.append(')');
        return b.toString();
    }

    public void serialize(Serializer s) throws IOException {
        s.writeInt(vals.length);
        for (int i=0; i<vals.length; i++) {
            s.writeExpression(vals[i]);
        }
    }

    public void deserialize(Deserializer s) throws IOException {
        vals=new Value[s.readInt()];
        for (int i=0; i<vals.length; i++) {
            vals[i]=(Value)s.readExpression();
        }
    }

    public boolean visit(ExpressionVisitor v) {
        for (int i=0; i<vals.length; i++) {
            if (!v.visit(vals[i])) return false;
        }
        return true;
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
