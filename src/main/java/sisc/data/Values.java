package sisc.data;

import java.io.*;
import sisc.interpreter.*;
import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;

public class Values extends Value {
    public Value[] values;

    public Values(Value[] v) {
        values=v;
    }

    public void display(ValueWriter w) throws IOException {
        for (int i=0; i<values.length-1; i++) {
            w.append(values[i]).append('\n');
        }
        if (values.length>0) w.append(values[values.length-1]);
    }

    public void eval(Interpreter r) throws ContinuationException {
        error(r, liMessage(SISCB,"multiplevalues"));
    }

    public boolean equals(Object v) {
        if (!(v instanceof Values)) return false;
        Values vs=(Values)v;
        if (vs.values.length!=values.length) return false;
        for (int i=values.length-1; i>=0; i--)
            if (!values[i].equals(vs.values[i])) 
                return false;
        return true;
    }

    public int hashCode() {
        int hc=0;
        for (int i=0; i<values.length; i++)
            hc^=values[i].hashCode();
        return hc;
    }
        
    public void serialize(Serializer s) throws IOException {
        s.writeInt(values.length);
        for (int i=0; i<values.length; i++) {
            s.writeExpression(values[i]);
        }
    }

    public Values() {}

    public void deserialize(Deserializer s) throws IOException {
        int size=s.readInt();
        values=new Value[size];
        for (int i=0; i<size; i++) {
            values[i]=(Value)s.readExpression();
        }
    }

    public boolean visit(ExpressionVisitor v) {
        for (int i=0; i<values.length; i++) {
            if (!v.visit(values[i])) return false;
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
