/*
 * Created on Nov 3, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package sisc.env;

import java.io.IOException;

import sisc.util.Util;
import sisc.data.Pair;
import sisc.data.Quantity;
import sisc.data.Value;
import sisc.interpreter.Interpreter;
import sisc.ser.Deserializer;
import sisc.ser.Serializer;

/**
 * @author scgmille
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public abstract class LexicalUtils {

    public static Value[] fixLexicals(Interpreter r, 
                                      int lcount, int[] locs, int[] lexs) {
        Value[] lexicals=r.createValues(lcount);
        int x=0;
        if (locs!=null)
            for (int i=locs.length-1; i>=0; i--) {
                int idx=locs[i];
                Value v=r.lcl[idx];
                lexicals[x++]=v;
            }
        if (lexs != null)
            for (int i=lexs.length-1; i>=0; i--) {
                int idx=lexs[i];
                Value v=r.env[idx];
                lexicals[x++]=v;
            }
        return lexicals;
    }

    public static Pair intArrayToList(int[] v) {
        if (v==null) return Util.EMPTYLIST;
        Pair rv=Util.EMPTYLIST;
        for (int i=0; i<v.length; i++) 
            rv=new Pair(Quantity.valueOf(v[i]), rv);
        return rv;
    }

    public static void writeIntArray(Serializer s, int[] v) throws IOException {
        if (v==null) s.writeInt(0);
        else {
            s.writeInt(v.length);
            for (int i=0; i<v.length; i++) {
                s.writeInt(v[i]);
            }
        }
    }

    public static int[] readIntArray(Deserializer s) throws IOException {
        int sz=s.readInt();
        if (sz>0) {
            int[] rv=new int[sz];
            for (int i=0; i<rv.length; i++) {
                rv[i]=s.readInt();
            }
            return rv;
        } else return null;
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
