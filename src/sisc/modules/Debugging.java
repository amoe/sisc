package sisc.modules;

import sisc.data.*;
import sisc.exprs.*;
import sisc.interpreter.*;
import sisc.nativefun.*;

import sisc.util.FreeReference;
import sisc.util.UndefinedVarException;

public class Debugging extends IndexedProcedure {

    //NEXT: 15
    protected static final int EXPRESSV = 0,
        CONT_VLR = 2,
        CONT_NXP = 3,
        CONT_ENV = 4,
        CONT_FK = 5,
        CONT_VLK = 6,
        CONT_PARENT = 7,
        CONT_STK = 14,
        ERROR_CONT_K = 8,
        FILLRIBQ = 9,
        FILLRIBEXP = 10,
        FREEXPQ = 11,
        FRESYM = 12,
        QTYPE = 13,
        UNRESOLVEDREFS = 1;

    public static class Index extends IndexedLibraryAdapter {

        public Value construct(Object context, int id) {
            return new Debugging(id);
        }
        
        public Index() {
            define("express", EXPRESSV);
            define("error-continuation-k", ERROR_CONT_K);
            define("continuation-vlk", CONT_VLK);
            define("continuation-vlr", CONT_VLR);
            define("continuation-nxp", CONT_NXP);
            define("continuation-env", CONT_ENV);
            define("continuation-fk", CONT_FK);
            define("continuation-stk", CONT_PARENT);
            define("continuation-stack-trace", CONT_STK);
            define("_fill-rib?", FILLRIBQ);
            define("_fill-rib-exp", FILLRIBEXP);
            define("_free-reference-exp?", FREEXPQ);
            define("_free-reference-symbol", FRESYM);
            define("quantity-type", QTYPE);
            define("unresolved-references", UNRESOLVEDREFS);
        }
    }
    
    public Debugging(int id) {
        super(id);
    }
    
    public Debugging() {}

    CallFrame getCont(Value v) {
        if (v instanceof ApplyParentFrame)
            return ((ApplyParentFrame)v).c;
        else
            return (CallFrame) v;
    }
    
    public Value doApply(Interpreter f) throws ContinuationException {
        switch(f.vlr.length) {
        case 0:
            switch(id) {
            case UNRESOLVEDREFS:
                FreeReference[] refs = FreeReference.allReferences();
                Pair res = EMPTYLIST;
                for (int i = 0; i < refs.length; i++) {
                    FreeReference ref = refs[i];
                    try {
                        ref.resolve();
                    } catch (UndefinedVarException ex) {
                        res = new Pair(ref.getName(), res);
                    }
                }
                return res;
            default:
                throwArgSizeException();
            }
        case 1:
            switch(id) {
            case QTYPE:
                return Quantity.valueOf(((Quantity) f.vlr[0]).type);
            case FREEXPQ:
                return truth(expr(f.vlr[0]) instanceof FreeReferenceExp);
            case FRESYM:
                return ((FreeReferenceExp)expr(f.vlr[0])).getSym();
            case FILLRIBQ:
                return truth(f.vlr[0] instanceof ExpressionValue &&
                             expr(f.vlr[0]) instanceof FillRibExp);
            case FILLRIBEXP:
                return new ExpressionValue(((FillRibExp)expr(f.vlr[0])).exp);
            case EXPRESSV:
                if (f.vlr[0] instanceof ExpressionValue) {
                    return expr(f.vlr[0]).express();
                } else {
                    return f.vlr[0].express();
                }
            case ERROR_CONT_K:
                return getCont(f.vlr[0]);
            case CONT_VLK:
                return truth(getCont(f.vlr[0]).vlk);
            case CONT_NXP:
                CallFrame cn=getCont(f.vlr[0]);
                if (cn.nxp==null) return FALSE;
                return new ExpressionValue(cn.nxp);
            case CONT_VLR:
                return new SchemeVector(getCont(f.vlr[0]).vlr);
            case CONT_ENV:
                return new Values(getCont(f.vlr[0]).env);
            case CONT_PARENT: 
                cn=getCont(f.vlr[0]);
                if (cn.parent==null) return FALSE;
                return cn.parent;
            case CONT_STK:
                cn=getCont(f.vlr[0]);
                return (cn.tracer == null) ? FALSE : cn.tracer.toValue();
            default:
                throwArgSizeException();
            }
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
