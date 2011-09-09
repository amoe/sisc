package sisc.modules;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.*;

public class Logical extends IndexedFixableProcedure {

    protected static final Symbol LOGICOPSB =
        Symbol.intern("sisc.modules.Messages");

    protected static final int 
        LOGAND = 1, LOGOR = 2, LOGXOR = 3, LOGNOT = 4, 
        LOGCOUNT = 5;

    public static class Index extends IndexedLibraryAdapter {

        public Value construct(Object context, int id) {
            return new Logical(id);
        }

        public Index() {
            define("logand", LOGAND);
            define("logor", LOGOR);
            define("logxor", LOGXOR);
            define("lognot", LOGNOT);
            define("logcount", LOGCOUNT);
        }
    }
    
    public Logical(int id) {
        super(id);
    }
    
    public Logical() {}
    
   public Value apply(Value v1) throws ContinuationException {
        switch(id) {
        case LOGNOT: return ((Quantity) v1).not();
        case LOGCOUNT: return ((Quantity) v1).bitCount();
        case LOGAND:
        case LOGOR:
        case LOGXOR:
            return (Quantity) v1;
        default:
            throwArgSizeException();
        }
        return VOID;
    }
    
    public Value apply(Value v1, Value v2) throws ContinuationException {
        switch(id) {
        case LOGAND:
            return ((Quantity) v1).and((Quantity) v2);
        case LOGOR:
            return ((Quantity) v1).or((Quantity) v2);
        case LOGXOR:
            return ((Quantity) v1).xor((Quantity) v2);
        default:
            throwArgSizeException();
        }
        return VOID;
    }

    public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
        switch(id) {
        case LOGAND:
            return ((Quantity) v1).and((Quantity) v2).and((Quantity) v3);
        case LOGOR:
            return ((Quantity) v1).or((Quantity) v2).or((Quantity) v3);
        case LOGXOR:
            return ((Quantity) v1).xor((Quantity) v2).xor((Quantity) v3);
        default:
            throwArgSizeException();
        }
        return VOID;
    }


    public Value apply(Value[] v) throws ContinuationException {
        Quantity r = (Quantity) v[0];
        switch(id) {
        case LOGAND:
            for (int i=v.length-1; i>0; i--)
                r=r.and((Quantity) v[i]);
            break;
        case LOGOR:
            for (int i=v.length-1; i>0; i--)
                r=r.or((Quantity) v[i]);
            break;
        case LOGXOR:
            for (int i=v.length-1; i>0; i--)
                r=r.xor((Quantity) v[i]);
            break;
        }
        return r;
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
