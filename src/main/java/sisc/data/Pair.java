package sisc.data;

import java.io.*;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;

public class Pair extends Value {
    private Value car, cdr;

    public Pair() {
        car=cdr=EMPTYLIST;
    }

    public Pair(Value car, Value cdr) {
        this.car=car;
        this.cdr=cdr;
    }

    public Value car() {
        return car;
    }

    public Value cdr() {
        return cdr;
    }

    public void setCar(Value v) {
        car = v;
    }

    public void setCdr(Value v) {
        cdr = v;
    }

    private static boolean quasiquote(ValueWriter w, String prefix, Value q, Value v) throws IOException {
        if (v instanceof Pair && ((Pair)v).cdr() == EMPTYLIST) {
            String qq = null;
            if (q==QUOTE) {
                qq = "'";
            } else if (q==UNQUOTE) {
                qq = ",";
            } else if (q==BACKQUOTE) {
                qq = "`";
            } else if (q==UNQUOTE_SPLICING) {
                qq = ",@";
            } else return false;
            w.append(prefix).append(qq).append(((Pair)v).car());
            return true;
        } else {
            return false;
        }
    }

    public void display(ValueWriter w) throws IOException {
        Value ccar = car;
        Value ccdr = cdr;
        if (quasiquote(w, "", ccar, ccdr)) return;
        w.append('(').append(ccar);
        while (ccdr != EMPTYLIST) {
            if ((ccdr instanceof Pair) && w.isInlinable(ccdr)) {
                Pair p = (Pair)ccdr;
                ccar = p.car();
                ccdr = p.cdr();
                if (quasiquote(w, " . ", ccar, ccdr)) break;
                w.append(' ').append(ccar);
            } else {
                w.append(" . ");
                w.append(ccdr);
                break;
            }
        }
        w.append(')');
    }

    public boolean valueEqual(Value v) {
        if (v==this) return true;
        if (!(v instanceof Pair)) return false;
        Pair p=(Pair)v;
        return car.valueEqual(p.car()) && cdr.valueEqual(p.cdr());
    }

    public int valueHashCode() {
        return car.valueHashCode() ^ cdr.valueHashCode();
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(car);
        s.writeExpression(cdr);
    }

    public void deserialize(Deserializer s) throws IOException {
        car=(Value)s.readExpression();
        cdr=(Value)s.readExpression();
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(car) && v.visit(cdr);
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
