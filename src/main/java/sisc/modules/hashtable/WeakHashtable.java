package sisc.modules.hashtable;

import sisc.data.*;

import java.util.Map;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

public class WeakHashtable extends Hashtable {

    private ReferenceQueue garbage = new ReferenceQueue();

    public WeakHashtable() {
        super();
    }

    public WeakHashtable(Procedure equalsProc, Procedure hashProc) {
        super(equalsProc, hashProc);
    }

    private class Key extends WeakReference implements HashtableKey {

        private int hash;

        public Key(Value key) {
            super(key, garbage);
            //we need to memoize the hash code so that it remains
            //available even after the key has become garbage
            hash = callHashCode(key);
        }

        public Value getValue() {
            return (Value)get();
        }

        public boolean equals(Object o) {
            //This first test is important: we need it in order to be
            //able to remove keys that have become garbage.
            if (o == this) return true;

            if (!(o instanceof Key)) return false;

            Value myVal = getValue();
            Value otherVal = ((Key)o).getValue();
            //A garbage key can never be equal to another garbage key
            return (myVal != null) && (otherVal != null) &&
                callEquals(myVal, otherVal);
        }

        public int hashCode() {
            return hash;
        }

    }

    protected HashtableKey makeKey(Value k) {
        return new Key(k);
    }

    protected Map getMap() {
        Map ht = super.getMap();
        Object r;
        while ((r = garbage.poll()) != null) {
            ht.remove(r);
        }

        return ht;
    }

    public boolean valueEqual(Value v) {
        if (!(v instanceof WeakHashtable)) return false;
        return super.valueEqual(v);
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
