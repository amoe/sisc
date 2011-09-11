package sisc.modules.hashtable;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.*;

public class Primitives extends IndexedFixableProcedure {

    public static final Symbol SHASHB =
        Symbol.intern("sisc.modules.hashtable.Messages");

    public static final HashtableBase shash(Value o) {
        try {
            return (HashtableBase)o;
        } catch (ClassCastException e) { typeError(SHASHB, "hashtable", o); }
        return null;
    }

    /**
     * The Simple procedures are purely functional procedures
     * which do not need to access interpreter registers to execute
     */
    public static class Simple extends IndexedFixableProcedure {
        public Simple() {}

        Simple(int id) {
            super(id);
        }
 
        public Value apply(Value v1) throws ContinuationException {
            switch (id) {
              case HTQ:
                  return SchemeBoolean.get(v1 instanceof HashtableBase);
              case HT_HASH_BY_EQ:
                  return Quantity.valueOf(System.identityHashCode(v1));
              case HT_HASH_BY_EQV:
                  return Quantity.valueOf(v1.hashCode());
              case HT_HASH_BY_EQUAL:
                  return Quantity.valueOf(v1.valueHashCode());
              case HT_HASH_BY_STRING_EQ:
                  return Quantity.valueOf(SchemeString.asString(v1).hashCode());
              case HT_HASH_BY_STRING_CI_EQ:
                  return Quantity.valueOf(SchemeString.asString(v1).toLowerCase().hashCode());
              default:
                  HashtableBase h = shash(v1);
                  switch(id) {
                    case HT_TO_ALIST:
                        return h.toAList();
                    case HT_KEYS:
                        return h.keys();
                    case HT_SIZE:
                        return Quantity.valueOf(h.size());
                    case HT_THREAD_SAFEQ:
                        return SchemeBoolean.get(h instanceof SynchronizedHashtable);
                    case HT_WEAKQ:
                        return SchemeBoolean.get((h instanceof WeakHashtable) ||
                                     ((h instanceof SynchronizedHashtable) &&
                                      ((SynchronizedHashtable)h).getDelegate()
                                      instanceof WeakHashtable));
                    case HT_EQUALSFN:
                        return h.getEqualsProc();
                    case HT_HASHFN:
                        return h.getHashProc();
                    default:
                        throwArgSizeException();
                        return VOID; //dummy
                  }
            }
        }

        public Value apply(Value v1, Value v2) throws ContinuationException {
            return apply(new Value[] {v1, v2});
        }

        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            return apply(new Value[] {v1, v2, v3}); 
        }

        public Value apply(Value[] v) throws ContinuationException {
            if (id == HT_MAKE) {
                if (v.length == 4) {
                    Procedure equalsProc = (Procedure) v[0];
                    Procedure hashProc = (Procedure) v[1];
                    HashtableBase res =
                        SchemeBoolean.toBoolean(v[3]) ?
                        new WeakHashtable(equalsProc, hashProc) :
                        new Hashtable(equalsProc, hashProc);
                    if (SchemeBoolean.toBoolean(v[2])) {
                        res = new SynchronizedHashtable(res);
                    }
                    return res;
                } else {
                    throwArgSizeException();
                }
            }

            Value def = FALSE;
            Value res = null;
            HashtableBase h = shash(v[0]);
            switch(id) {
              case HT_GET:
                  switch (v.length) {
                    case 2: break;
                    case 3: def = v[2]; break;
                    default:
                        throwArgSizeException();
                  }
                  res = h.get(v[1]);
                  break;
              default:
                  throwArgSizeException();
            }
            return (res == null) ? def : res;
        }
    }
    
    /**
     * The Complex procedures either have a side effect, or
     * require the interpreter to execute
     */
    public static class Complex extends CommonIndexedProcedure {
        public Complex() {}
        
        Complex(int id) {
            super(id);
        }

        public Value apply(Value v1) throws ContinuationException {
            HashtableBase h = shash(v1);
            switch(id) {
              case HT_CLEAR:
                  h.clear();
                  return VOID;
              default:
                  throwArgSizeException();
            }
            return VOID; //dummy
        }            

        public Value apply(Value v1, Value v2) throws ContinuationException {
            return apply(new Value[] {v1, v2});
        }

        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            return apply(new Value[] {v1, v2, v3}); 
        }

        public Value apply(Value[] v) throws ContinuationException {
            Value def = FALSE;
            Value res = null;
            HashtableBase h = shash(v[0]);
            switch(id) {
              case HT_PUT:
                  switch (v.length) {
                    case 3: break;
                    case 4: def = v[3]; break;
                    default:
                        throwArgSizeException();
                  }
                  res = h.put(v[1], v[2]);
                  break;
              case HT_REMOVE:
                  switch (v.length) {
                    case 2: break;
                    case 3: def = v[2]; break;
                    default:
                        throwArgSizeException();
                  }
                  res = h.remove(v[1]);
                  break;
              case HT_ADD_ALIST:
                  switch (v.length) {
                    case 2: h.addAList((Pair) v[1]); return h;
                    default:
                        throwArgSizeException();
                  }
              default:
                  throwArgSizeException();
            }
            return (res == null) ? def : res;
        }
    }
    
    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {

        public Index() {
            define("hashtable/make", HT_MAKE);
            define("hashtable?", HTQ);
            define("hashtable/put!", Complex.class, HT_PUT);
            define("hashtable/get", HT_GET);
            define("hashtable/remove!", Complex.class, HT_REMOVE);
            define("hashtable/clear!", Complex.class, HT_CLEAR);
            define("hashtable/size", HT_SIZE);
            define("hashtable->alist", HT_TO_ALIST);
            define("hashtable/add-alist!", Complex.class, HT_ADD_ALIST);
            define("hashtable/keys", HT_KEYS);
            define("hashtable/thread-safe?", HT_THREAD_SAFEQ);
            define("hashtable/weak?", HT_WEAKQ);
            define("hashtable/equivalence-function", HT_EQUALSFN);
            define("hashtable/hash-function", HT_HASHFN);
            define("hash-by-eq", HT_HASH_BY_EQ);
            define("hash-by-eqv", HT_HASH_BY_EQV);
            define("hash-by-equal", HT_HASH_BY_EQUAL);
            define("hash-by-string=", HT_HASH_BY_STRING_EQ);
            define("hash-by-string-ci=", HT_HASH_BY_STRING_CI_EQ);
        }
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
        }
        
    }

    protected static final int
        //NEXT = 19,
        HT_MAKE = 0,
        HTQ = 4,
        HT_PUT = 5,
        HT_GET = 6,
        HT_REMOVE = 7,
        HT_CLEAR = 8,
        HT_SIZE = 9,
        HT_TO_ALIST = 10,
        HT_ADD_ALIST = 11,
        HT_KEYS = 12,
        HT_THREAD_SAFEQ = 13,
        HT_WEAKQ = 14,
        HT_HASH_BY_EQ = 1,
        HT_HASH_BY_EQV = 2,
        HT_HASH_BY_EQUAL = 3,
        HT_HASH_BY_STRING_EQ = 17,
        HT_HASH_BY_STRING_CI_EQ = 18,
        HT_EQUALSFN = 15,
        HT_HASHFN = 16;
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
