package sisc.modules.s2j;

import java.lang.reflect.*;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.IndexedLibraryAdapter;

public class Conversion extends Util {

    protected static final int CONV_JBOOLEAN = 1,
        CONV_JCHAR = 2,
        CONV_JBYTE = 3,
        CONV_JSHORT = 4,
        CONV_JINT = 5,
        CONV_JLONG = 6,
        CONV_JFLOAT = 7,
        CONV_JDOUBLE = 8,
        CONV_JSTRING = 9,
        CONV_JARRAY = 10,
        CONV_BOOLEAN = 20,
        CONV_CHARACTER = 21,
        CONV_NUMBER = 22,
        CONV_STRING = 23,
        CONV_SYMBOL = 24,
        CONV_VECTOR = 25,
        CONV_LIST = 26;

    public static class Index extends IndexedLibraryAdapter {

        public Value construct(Object context, int id) {
            return new Conversion(id);
        }

        public Index() {
            define("->jboolean", CONV_JBOOLEAN);
            define("->jchar", CONV_JCHAR);
            define("->jbyte", CONV_JBYTE);
            define("->jshort", CONV_JSHORT);
            define("->jint", CONV_JINT);
            define("->jlong", CONV_JLONG);
            define("->jfloat", CONV_JFLOAT);
            define("->jdouble", CONV_JDOUBLE);
            define("->jstring", CONV_JSTRING);
            define("->jarray", CONV_JARRAY);

            define("->boolean", CONV_BOOLEAN);
            define("->character", CONV_CHARACTER);
            define("->number", CONV_NUMBER);
            define("->string", CONV_STRING);
            define("->symbol", CONV_SYMBOL);
            define("->vector", CONV_VECTOR);
            define("->list", CONV_LIST);
        }
    }
 
    public Conversion(int id) {
        super(id);
    }
    
    public Conversion() {} 
   
    public Value apply(Value v1) throws ContinuationException {
        switch(id) {
        case CONV_JBOOLEAN:
            return makeJObj((truth(v1) ? Boolean.TRUE : Boolean.FALSE), Boolean.TYPE);
        case CONV_JCHAR:
            return makeJObj(new Character(character(v1)), Character.TYPE);
        case CONV_JBYTE:
            return makeJObj(new Byte((byte)((Quantity) v1).intValue()), Byte.TYPE);
        case CONV_JSHORT:
            return makeJObj(new Short((short)((Quantity) v1).intValue()), Short.TYPE);
        case CONV_JINT:
            return makeJObj(new Integer(((Quantity) v1).intValue()), Integer.TYPE);
        case CONV_JLONG:
            return makeJObj(new Long(((Quantity) v1).longValue()), Long.TYPE);
        case CONV_JFLOAT:
            return makeJObj(new Float((float)((Quantity) v1).doubleValue()), Float.TYPE);
        case CONV_JDOUBLE:
            return makeJObj(new Double(((Quantity) v1).doubleValue()), Double.TYPE);
        case CONV_JSTRING:
            Value v = v1;
            if (v instanceof Symbol)
                return makeJObj(symval(v));
            else if (v instanceof SchemeString)
                return makeJObj(string(v));
            else
                typeError(S2JB, "stringorsymbol", v);
        case CONV_BOOLEAN:
            return truth(((Boolean)jobj(v1)).booleanValue());
        case CONV_CHARACTER:
            return new SchemeCharacter(((Character)jobj(v1)).charValue());
        case CONV_NUMBER:
            Object o = jobj(v1);
            if (o instanceof Byte ||
                o instanceof Short ||
                o instanceof Integer ||
                o instanceof Long)
                return Quantity.valueOf(((Number)o).longValue());
            else if (o instanceof Float ||
                     o instanceof Double)
                return Quantity.valueOf(((Number)o).doubleValue());
            else if (o instanceof java.math.BigInteger)
                return Quantity.valueOf((java.math.BigInteger)o);
            else if (o instanceof java.math.BigDecimal)
                return Quantity.valueOf((java.math.BigDecimal)o);
            else
                typeError(S2JB, "jnumber", v1);
        case CONV_STRING:
            o = jobj(v1);
            if (o instanceof String)
                return new SchemeString((String)o);
            else
                return new SchemeString(o.toString());
        case CONV_SYMBOL:
            o = jobj(v1);
            if (o instanceof String)
                return Symbol.intern((String)o);
            else
                return Symbol.intern(o.toString());
        case CONV_VECTOR:
            o = jobj(v1);
            if (o.getClass().isArray()) {
                Class componentType = o.getClass().getComponentType();
                Value[] vals = new Value[Array.getLength(o)];
                for (int i=0; i < vals.length; i++) {
                    vals[i] = makeJObj(Array.get(o, i), componentType);
                }
                return new SchemeVector(vals);
            } else
                typeError(S2JB, "jarray", v1);
        case CONV_LIST:
            o = jobj(v1);
            if (o.getClass().isArray()) {
                Class componentType = o.getClass().getComponentType();
                Pair p = EMPTYLIST;
                for (int i=Array.getLength(o)-1; i>=0; i--) {
                    p = new Pair(makeJObj(Array.get(o, i), componentType), p);
                }
                return p;
            } else
                typeError(S2JB, "jarray", v1);
        default:
            throwArgSizeException();
        }
        return VOID;
    }
    
    public Value apply(Value v1, Value v2) throws ContinuationException {
        switch(id) {
        case CONV_JARRAY:
            Value o = v1;
            Value[] vals = null;
            if (o instanceof Pair)
                vals = pairToValues(pair(o));
            else if (o instanceof SchemeVector)
                vals = vec(o).vals;
            else {
                typeError(S2JB, "listorvector", o);
            }
            Object res = Array.newInstance(jclass(v2), vals.length);
            for (int i=0; i< vals.length; i++) {
                Array.set(res, i, jobj(vals[i]));
            }
            return makeJObj(res);
        default:
            throwArgSizeException();
        }
        return VOID;
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

