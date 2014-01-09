package sisc.compiler;

import java.util.*;
import sisc.data.*;
import sisc.util.Util;

class ARecord extends Util {
    static final Symbol program=Symbol.get("program");

    Map references=new HashMap();
    Map sets=new HashMap();
    Set frees=new HashSet();
    Value body;

    public ARecord(Value v) {
        body=v;
    }

    public String toString() {
        return "{refs: " + references + ", sets: "+ sets + ", frees:"+frees+"}";
    }

    protected static final Pair mapToAssoc(Map m) {
        Pair rv=EMPTYLIST;
        for (Iterator i=m.keySet().iterator(); i.hasNext();) {
            Value key=(Value)i.next();
            rv=new Pair(new Pair(key, Quantity.valueOf(((Integer)m.get(key)).intValue())),
                        rv);
        }
        return rv;
    }

    protected static final Pair setToList(Set s) {
        Pair rv=EMPTYLIST;
        for (Iterator i=s.iterator(); i.hasNext();) {
            rv=new Pair((Value)i.next(), rv);
        }
        return rv;
    }

    public Expression asExpression() {
        return new Pair(program, new Pair(mapToAssoc(references),
                                          list(mapToAssoc(sets),
                                               setToList(frees),
                                               body)));
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

