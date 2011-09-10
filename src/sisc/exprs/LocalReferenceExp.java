package sisc.exprs;

import java.io.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class LocalReferenceExp extends Expression implements Immediate {

    public int idx;

    public LocalReferenceExp(int idx) {
        this.idx=idx;
    }

    public void eval(Interpreter r) throws ContinuationException {
        r.acc=r.lcl[idx];
        r.nxp=null;
        //r.lc++;
    }

    public final Value getValue(Interpreter r) throws ContinuationException {
        //r.lc++;
        return r.lcl[idx];
    }

    public Value express() {
        return list(Symbol.get("lcl"), Quantity.valueOf(idx));
    }

    public void serialize(Serializer s) throws IOException {
        s.writeInt(idx);
    }

    public LocalReferenceExp() {}

    public void deserialize(Deserializer s) throws IOException {
        idx=s.readInt();
    }

    public boolean equals(Object o) {
        if (!(o instanceof LocalReferenceExp))
            return false;
        return idx==((LocalReferenceExp)o).idx;
    }

    public int hashCode() {
        return idx | 0xea000000;
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
