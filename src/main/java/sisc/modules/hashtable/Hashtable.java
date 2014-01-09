package sisc.modules.hashtable;

import sisc.data.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.io.IOException;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.util.Util;
import sisc.nativefun.FixableProcedure;
import sisc.interpreter.Interpreter;
import sisc.interpreter.Context;
import sisc.interpreter.ContinuationException;
import sisc.interpreter.SchemeException;

public class Hashtable extends HashtableBase {

    private HashMap ht;
    private Pair alist;//tmp store during deserialisation;

    private Procedure equalsProc;
    private Procedure hashProc;

    public Hashtable() {
        this.ht = new HashMap(0);
    }

    public Hashtable(Procedure equalsProc, Procedure hashProc) {
        this();
        this.equalsProc = equalsProc;
        this.hashProc = hashProc;
    }

    public Procedure getEqualsProc() {
        return equalsProc;
    }

    public Procedure getHashProc() {
        return hashProc;
    }

    public boolean callEquals(Value v1, Value v2) {
        Value rv = Util.VOID;
        if (equalsProc instanceof FixableProcedure) {
            try {
                rv = ((FixableProcedure)equalsProc).apply(v1, v2);
            } catch (ContinuationException ce) {
                //TODO: handle this error better?
                Procedure.throwPrimException(ce.getMessage());
            }
        } else {
            Interpreter r = Context.enter();
            try {
                rv = r.eval(equalsProc, new Value[] { v1, v2 });
            } catch (SchemeException e) {
                Procedure.throwNestedPrimException
                    (Util.liMessage(Primitives.SHASHB,
                                    "equalsexception",
                                    equalsProc.toString()),
                     e);
            } finally {
                Context.exit();
            }
        }

        if (rv instanceof SchemeBoolean) {
            return SchemeBoolean.toBoolean(rv);
        } else {
            Procedure.throwPrimException
                (Util.liMessage(Primitives.SHASHB, "equalsreturn",
                                equalsProc.toString(),
                                rv.synopsis()));
            return false; //dummy
        }
    }

    public int callHashCode(Value v) {
        Value rv = Util.VOID;

        if (hashProc instanceof FixableProcedure) {
            try {
                rv = ((FixableProcedure)hashProc).apply(v);
            } catch (ContinuationException ce) {
                //TODO: handle this error better?
                Procedure.throwPrimException(ce.getMessage());
            }
        } else {
            Interpreter r = Context.enter();
            try {
                rv = r.eval(hashProc, new Value[] { v });
            } catch (SchemeException e) {
                Procedure.throwNestedPrimException
                    (Util.liMessage(Primitives.SHASHB,
                                    "hashexception",
                                    hashProc.toString()),
                     e);
            } finally {
                Context.exit();
            }
        }

        if (rv instanceof Quantity) {
            return ((Quantity)rv).intValue();
        } else {
            Procedure.throwPrimException
                (Util.liMessage(Primitives.SHASHB, "hashreturn",
                                hashProc.toString(),
                                rv.synopsis()));
            return 0; //dummy
        }
    }


    private class Key implements HashtableKey {

        private Value key;

        public Key(Value key) {
            this.key = key;
        }

        public Value getValue() {
            return key;
        }

        public boolean equals(Object o) {
            return (o instanceof Key) && callEquals(key, ((Key)o).key);
        }

        public int hashCode() {
            return callHashCode(key);
        }

    }

    protected HashtableKey makeKey(Value k) {
        return new Key(k);
    }

    protected Map getMap() {
        if (alist != null) {
            addAList(ht, alist);
            alist = null;
        }
        return ht;
    }

    private void addAList(Map m, Pair p) {
        for (; p != EMPTYLIST; p = (Pair) p.cdr()) {
            Pair entry = (Pair) p.car();
            m.put(makeKey(entry.car()), entry.cdr());
        }
    }

    //NB: getKey is allowed to return null, indicating that the key is
    //no longer valid. This is never the case in this class, but
    //sub-classes, notably WeakHashtable, exploit this feature.
    private Value getKey(Object o) {
        return ((HashtableKey)o).getValue();
    }

    private Value getMapKey(Map.Entry e) {
        return getKey(e.getKey());
    }

    private Value getMapValue(Map.Entry e) {
        return (Value)e.getValue();
    }

    public Value get(Value k) {
        return (Value)getMap().get(makeKey(k));
    }

    public Value put(Value k, Value v) {
        return (Value)getMap().put(makeKey(k), v);
    }

    public Value remove(Value k) {
        return (Value)getMap().remove(makeKey(k));
    }

    public int size() {
        return getMap().size();
    }

    public void clear() {
        getMap().clear();
    }

    public void addAList(Pair p) {
        Map m = getMap();
        addAList(m, p);
    }

    public Pair toAList() {
        Iterator i = getMap().entrySet().iterator();
        Pair res = EMPTYLIST;
        while(i.hasNext()) {
            Map.Entry e = (Map.Entry)i.next();
            Value key = getMapKey(e);
            Value val = getMapValue(e);
            if (key != null) {
                res = new Pair(new Pair(key, val), res);
            }
        }
        return res;
    }

    public Pair keys() {
        Iterator i = getMap().keySet().iterator();
        Pair res = EMPTYLIST;
        while(i.hasNext()) {
            Value key = getKey(i.next());
            if (key != null) {
                res = new Pair(key, res);
            }
        }
        return res;
    }

    public boolean valueEqual(Value v) {
        if (v==this) return true;
        if (!(v instanceof Hashtable)) return false;
        Hashtable o = (Hashtable)v;
        if (size() != o.size()) return false;
        if (!equalsProc.valueEqual(o.equalsProc) ||
            !hashProc.valueEqual(o.hashProc)) return false;
        for (Iterator i = getMap().entrySet().iterator(); i.hasNext();) {
            Map.Entry e = (Map.Entry)i.next();
            Value key = getMapKey(e);
            Value val = getMapValue(e);
            if (key == null || !val.valueEqual(o.get(key))) return false;
        }
        return true;
    }

    public int valueHashCode() {
        int res = equalsProc.valueHashCode() ^ hashProc.valueHashCode();
        for (Iterator i = getMap().entrySet().iterator(); i.hasNext();) {
            Map.Entry e = (Map.Entry)i.next();
            Value key = getMapKey(e);
            Value val = getMapValue(e);
            if (key != null) {
                res += key.valueHashCode() ^ val.valueHashCode();
            }
        }
        return res;
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(equalsProc);
        s.writeExpression(hashProc);
        s.writeExpression(toAList());
    }

    public void deserialize(Deserializer s) throws IOException {
        equalsProc = (Procedure)s.readExpression();
        hashProc   = (Procedure)s.readExpression();
        alist      = (Pair)s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        if (!v.visit(equalsProc) || !v.visit(hashProc)) return false;
        Iterator i = getMap().entrySet().iterator();
        while(i.hasNext()) {
            Map.Entry e = (Map.Entry)i.next();
            Value key = getMapKey(e);
            Value val = getMapValue(e);
            if (key != null) {
                if (!v.visit(key)) return false;
                if (!v.visit(val)) return false;
            }
        }
        return true;
    }

    public void display(ValueWriter w) throws IOException {
        w.append("#<")
            .append(Util.liMessage(Primitives.SHASHB, "hashtable"))
            .append(' ')
            .append(equalsProc)
            .append(' ')
            .append(hashProc)
            .append(" (");
        Iterator i = getMap().entrySet().iterator();
        while(i.hasNext()) {
            Map.Entry e = (Map.Entry)i.next();
            Value key = getMapKey(e);
            Value val = getMapValue(e);
            if (key != null) {
                w.append('(')
                    .append(key).append(" . ").append(val)
                    .append(')');
            }
        }
        w.append(")>");
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
