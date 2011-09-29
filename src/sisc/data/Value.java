package sisc.data;

import java.io.*;
import sisc.interpreter.*;
import sisc.io.SharedValueWriter;
import sisc.io.ValueWriter;
import sisc.util.Defaults;
import sisc.util.InternedValue;

/**
 * Value is the base class for anything treated as a first-class value 
 * within Scheme.
 */
public abstract class Value extends Expression implements Immediate {

    public abstract void display(ValueWriter w) throws IOException;

    public void apply(Interpreter r) throws ContinuationException {
        error(r, liMessage(SISCB,"nonprocapp", synopsis()));
    }

    public String synopsis() {        
        Interpreter r = Context.currentInterpreter();
        return synopsis(r == null ?
                        Defaults.SYNOPSIS_LENGTH :
                        r.dynenv.synopsisLength);
    }

    /**
     * A synopsis is a limited number of characters of a human
     * readable Value representation. They are used in errors or
     * informational messages where the entire value's representation
     * is not necessary.
     *
     * @param limit The number of characters to display
     */
    public String synopsis(int limit) {
        String v=toString();
        if (v.length() > limit)
            return v.substring(0,limit)+"...";
        else return v;
    }

    public void write(ValueWriter w) throws IOException {
        display(w);
    }

    /**
     * Compares this Value to another. By default equal to eqv.
     * 
     * @param v the other Value
     */
    public boolean equals(Object v) {
        return eqv(v);
    }

    /**
     * Compares this Value to another for equality according to the
     * rules of Scheme's eqv?. In SISC, this differs from eq? in that
     * eq? is strict pointer equality.
     * 
     * @param v the other Value
     */
    public boolean eqv(Object v) {
        return this==v;
    }

    /**
     * Compares this value to another for semantic equality.  Used to
     * implement Scheme's 'equal?'.  Subclasses <b>must</b> test for
     * pointer equality as well as semantic equality, so that infinite loops 
     * on recursive structures are less likely.
     * 
     * @param v the other Value
     */
    public boolean valueEqual(Value v) {
        return eqv(v) || equals(v);
    }

    /**
     * a hashCode function consistent with valueEqual
     *
     * @return hash code
     */
    public int valueHashCode() {
        return hashCode();
    }

    public String toString() {
        StringWriter sw = new StringWriter();
        ValueWriter w = new SharedValueWriter(sw, false, false);
        try {
            w.write(this);
            sw.close();
        } catch (IOException e) {
            //shouldn't happen since we are writing to string
        }
        return sw.toString();
    }

    /**
     * Called to evaluate this value. As Values ordinarily evaluate
     * to themselves, this method simply sets <tt>acc</tt> to this.
     * 
     * @param r 
     * @exception ContinuationException 
     */
    public void eval(Interpreter r) throws ContinuationException {
        r.acc=this;
        r.nxp=null;
    }

    /**
     * Called to obtain the value of this Value. Simply <tt>this</tt>.
     * 
     * @param r 
     * @exception ContinuationException 
     */
    public Value getValue(Interpreter r) throws ContinuationException {
        return this;
    }

    public Value express() {
        return list(Symbol.get("val"), this);
    }

    /**
     * Helper function to generate an opaque type representation which
     * may be named
     */
    public void displayNamedOpaque(ValueWriter w, String type)
        throws IOException {
        w.append("#<").append(type);

        Symbol name=getName();
        if (name!=null)
            w.append(' ').append(name);
        w.append('>');
    }

    public Object writeReplace() throws ObjectStreamException {
        InternedValue iv = InternedValue.lookupByValue(this);
        return (iv == null) ? (Object)this : (Object)iv;
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
