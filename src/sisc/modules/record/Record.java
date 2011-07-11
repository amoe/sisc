package sisc.modules.record;

import sisc.data.*;
import java.io.IOException;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.util.Util;

public class Record extends Value {

    private Value type;
    private Value[] slots;

    public Record() {}

    public Record(Value type, int sz) {
        this.type = type;
        this.slots = new Value[sz];
        for (int i=0; i<sz; i++) slots[i] = VOID;
    }

    public Value getType() { return type; }
    public void setType(Value v) { type = v; }
    public Value getSlot(int idx) { return slots[idx]; }
    public void setSlot(int idx, Value v) { slots[idx] = v; }
    public int size() { return slots.length; }

    public boolean valueEqual(Value v) {
        if (v==this) return true;
        if (!(v instanceof Record)) return false;
        Record r = (Record)v;
        if (type != r.getType() || size() != r.size()) return false;
        for (int i=0; i<slots.length; i++) {
            if (!slots[i].valueEqual(r.getSlot(i))) return false;
        }
        return true;
    }

    public int valueHashCode() {
        int res = System.identityHashCode(type);
        for (int i=0; i<slots.length; i++) {
            res ^= slots[i].valueHashCode();
        }
        return res;
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(type);
        s.writeInt(size());
        for (int i=0; i<slots.length; i++) {
            s.writeExpression(slots[i]);
        }
    }

    public void deserialize(Deserializer s) throws IOException {
        type = (Value)s.readExpression();
        slots = new Value[s.readInt()];
        for (int i=0; i<slots.length; i++) {
            slots[i] = (Value)s.readExpression();
        }
    }

    public boolean visit(ExpressionVisitor v) {
        if (!v.visit(type)) return false;
        for (int i=0; i<slots.length; i++) {
            if (!v.visit(slots[i])) return false;
        }
        return true;
    }

    public void display(ValueWriter w) throws IOException {
        w.append("#<")
            .append(Util.liMessage(Primitives.SRECORDB, "record"))
            .append(' ')
            .append(type)
            .append(" [");
        for (int i=0; i<slots.length-1; i++) {
            w.append(slots[i]).append(' ');
        }
        if (slots.length>0) w.append(slots[slots.length-1]);
        w.append("]>");
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
