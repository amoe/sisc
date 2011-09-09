package sisc.modules.record;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.util.Util;

public abstract class Primitives extends Util {

    public static final Symbol SRECORDB =
        Symbol.intern("sisc.modules.record.Messages");

    protected static final int
        RECORD_MAKE = 1,
        RECORDQ = 2,
        RECORD_TYPE = 3,
        RECORD_SET_TYPE = 4,
        RECORD_REF = 5,
        RECORD_SET = 6;

    public static final Record record(Value o) {
        try {
            return (Record)o;
        } catch (ClassCastException e) { typeError(SRECORDB, "record", o); }
        return null;
    }

    /**
     * The Simple procedures are purely functional procedures
     * which do not need to access interpreter registers to execute
     */
    public static class Simple extends IndexedFixableProcedure {
        public Simple() {}

        Simple(int id) {
            super(id);
        }

        public Value apply(Value v1) throws ContinuationException {
            switch (id) {
            case RECORDQ:
                return truth(v1 instanceof Record);
            case RECORD_TYPE:
                return record(v1).getType();
            default:
                throwArgSizeException();
            }
            return VOID;
        }

        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch (id) {
            case RECORD_MAKE:
                return new Record(v1, ((Quantity) v2).indexValue());
            case RECORD_REF:
                return record(v1).getSlot(((Quantity) v2).indexValue());
            default:
                throwArgSizeException();
            }
            return VOID;
        }
    }
    
    /**
     * The Complex procedures either have a side effect, or
     * require the interpreter to execute
     */
    public static class Complex extends CommonIndexedProcedure {
        public Complex() {}
     
        Complex(int id) {
            super(id);
        }

        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch (id) {
            case RECORD_SET_TYPE:
                record(v1).setType(v2);
                return VOID;
            default:
                throwArgSizeException();
            }
            return VOID;
        }

        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            switch (id) {
            case RECORD_SET:
                try {
                    record(v1).setSlot(((Quantity) v2).indexValue(), v3);
                } catch (ArrayIndexOutOfBoundsException aib) {
                    throwPrimException(liMessage(SRECORDB, "nosuchslot", ((Quantity) v2).toString(), record(v1).synopsis()));
                }
                return VOID;
            default:
                throwArgSizeException();
            }
            return VOID;
        }
    }
    
    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {

        public Index() {
            define("make-record", RECORD_MAKE);
            define("record?", RECORDQ);
            define("record-type", RECORD_TYPE);
            define("record-type!", Complex.class, RECORD_SET_TYPE);
            define("record-ref", RECORD_REF);
            define("record-set!", Complex.class, RECORD_SET);
        }
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
        }
        
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
