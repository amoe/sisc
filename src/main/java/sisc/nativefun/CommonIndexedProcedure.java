package sisc.nativefun;

import sisc.data.Value;
import sisc.interpreter.ContinuationException;
import sisc.interpreter.Interpreter;

/**
 * CommonIndexedProcedure is a helper class intended to unify the interfaces
 * of fixable and non-fixable procedures, so that development of native
 * libraries is more consistent and flexible.
 */
public abstract class CommonIndexedProcedure extends IndexedProcedure {
    
    public CommonIndexedProcedure() {}

    public CommonIndexedProcedure(int id) {
        super(id);
    }

    /**
     * A common indexed procedure must subclass one of the following methods
     */
    public Value applyZero(Interpreter r) throws ContinuationException {
        return apply();
    }

    public Value apply() throws ContinuationException {
        throwArgSizeException();
        return VOID;
    }
    
    public Value apply(Interpreter r, Value v1) throws ContinuationException {
        return apply(v1);
    }

    public Value apply(Value v1) throws ContinuationException {
        throwArgSizeException();
        return VOID;
    }

    public Value apply(Interpreter r, Value v1, Value v2) throws ContinuationException {
        return apply(v1, v2);
    }

    public Value apply(Value v1, Value v2) throws ContinuationException {
        throwArgSizeException();
        return VOID;
    }

    public Value apply(Interpreter r, Value v1, Value v2, Value v3) 
        throws ContinuationException {
        return apply(v1, v2, v3);
    }

    public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
        throwArgSizeException();
        return VOID;
    }

    public Value apply(Interpreter r, Value v[]) 
        throws ContinuationException {
        return apply(v);
    }

    public Value apply(Value vlr[]) throws ContinuationException {
        throwArgSizeException();
        return VOID;
    }

    public Value doApply(Interpreter r) throws ContinuationException {
        switch (r.vlr.length) {
        case 0:
            return applyZero(r);
        case 1:
            return apply(r, r.vlr[0]);
        case 2:
            return apply(r, r.vlr[0], r.vlr[1]);
        case 3:
            return apply(r, r.vlr[0], r.vlr[1], r.vlr[2]);
        default:
            return apply(r, r.vlr);
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
