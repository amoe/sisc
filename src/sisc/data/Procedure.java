package sisc.data;

import sisc.interpreter.*;
import sisc.nativefun.NestedPrimRuntimeException;
import sisc.nativefun.PrimRuntimeException;
/**
 * The Procedure class is the base class for any Scheme Procedure.  
 *
 * A procedure is an entity that can be applied to zero or more
 * arguments to return a value or cause additional expressions to be
 * evaluated.
 */
public abstract class Procedure extends Value {

    /**
     * Called when applying this procedure to a number of arguments in 
     * the Interpreter's <tt>vlr</tt> register. The code can return a
     * value by setting the Interpreter's <tt>acc</tt> register and/or
     * cause additional expressions to be evaluated by setting the
     * Interpreter's <tt>nxp</tt> register.
     * 
     * @param r the Interpreter 
     * @exception ContinuationException 
     */
    public abstract void apply(Interpreter r) throws ContinuationException;

    public static void throwPrimException(String message) {
        throw new PrimRuntimeException(message);
    }

    public static void throwNestedPrimException(String message,
                                                SchemeException e) {
        throw new NestedPrimRuntimeException(message, e);
    }

    public static void throwNestedPrimException(SchemeException e) {
        throw new NestedPrimRuntimeException(e);
    }

    public static void throwArgSizeException() {
        throwPrimException(liMessage(SISCB, "incorrectargcount"));
    }
    
    public static void error(Interpreter r,
                             Value where,
                             NestedPrimRuntimeException parent)
        throws ContinuationException {
        SchemeException rootCauseException = parent.getRootCause();
        Pair rootCause =
            new Pair(
                new Pair(ERRORK, rootCauseException.e),
                new Pair(
                    new Pair(FCONT, rootCauseException.f),
                    rootCauseException.m));
        String parentMessage = parent.getMessage();
        error(r,
            (parentMessage == null
                ? list(new Pair(LOCATION, where), new Pair(PARENT, rootCause))
                : list(
                    new Pair(MESSAGE, new SchemeString(parentMessage)),
                    new Pair(LOCATION, where),
                    new Pair(PARENT, rootCause))));
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
