package sisc.exprs;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.ser.*;
import java.io.IOException;

public class AppEval extends Expression {
    
    public AppEval() {}

    public final void eval(Interpreter r) throws ContinuationException {

        r.trace(this);

        /* To allow break of execution (turadg)
         */
        if (permitInterrupts && r.tctx.interrupt) {
            /*
              We bring the stack into a state s.t. when it is captured
              by error() below, and later resumed, it will continue
              execution exactly where we left off. We push the current
              AppEval, and the acc. When the error k is invoked, the
              pushed acc will self-evaluate, thus placing itself back
              in the acc. The following pop brings back the AppEval,
              thus getting us exactly back to the point where
              execution was interrupted.
            */
            r.push(this);
            r.push(r.acc);
            r.tctx.interrupt = false;
            error(r, liMessage(SISCB, "evaluationinterrupted"));
        }

        r.acc.apply(r);
    }

    public Value express() {
        return list(Symbol.get("appl"));
    }

    public boolean equals(Object o) {
        if (!(o instanceof AppEval)) return false;
        AppEval other=(AppEval)o;
        return (annotations!=null ? 
                annotations.equals(other.annotations) :
                other.annotations == null);
    }

    public int hashCode() {
        return 0x37895f61 ^ (annotations == null ? 0 : 
                             annotations.hashCode());
    }

    public void serialize(Serializer s) throws IOException {
    }

    public void deserialize(Deserializer s) throws IOException {
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
