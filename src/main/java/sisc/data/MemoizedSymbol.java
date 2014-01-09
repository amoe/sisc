package sisc.data;

import java.util.WeakHashMap;
import java.lang.ref.WeakReference;

public class MemoizedSymbol extends Symbol implements Singleton {

    private static WeakHashMap memo=new WeakHashMap(100);

    private MemoizedSymbol(String symval) {
        super(symval);
    }

    /**
     * Interns the given symbol.
     * @return the interned symbol, or null if the symbol was not
     * previously defined
     */
    public static Symbol intern(String str) {
        synchronized(memo) {
            WeakReference wr=(WeakReference)memo.get(str);
            Symbol s=(wr == null ? null : (Symbol)wr.get());
            if (s==null) {
                s=new MemoizedSymbol(str);
                memo.put(str, new WeakReference(s));
            }
            return s;
        }
    }

    public MemoizedSymbol() {}

    public Value singletonValue() {
        return intern(symval);
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
