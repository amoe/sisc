package sisc.ser;

import java.io.ObjectOutput;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

import sisc.data.Expression;
import sisc.env.SymbolicEnvironment;

public interface Serializer extends ObjectOutput {

    void writeBigDecimal(BigDecimal d) throws IOException;

    void writeBigInteger(BigInteger i) throws IOException;

    void writeExpression(Expression e) throws IOException;
    void writeExpressionArray(Expression[] vlr) throws IOException;
    void writeInitializedExpression(Expression e) throws IOException;

    void writeSymbolicEnvironment(SymbolicEnvironment e) throws IOException;
    void writeClass(Class c) throws IOException;
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
