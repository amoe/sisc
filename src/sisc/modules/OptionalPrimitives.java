package sisc.modules;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.util.Util;

public class OptionalPrimitives extends Util {

    static long symid = 0;

    public static final Value cadr(Value p) {
        return ((Pair) ((Pair)p).cdr()).car();
    }


    public static final Value cddr(Value p) {
        return ((Pair)((Pair)p).cdr()).cdr();
    }

    public static boolean jnumQuery(Value v, int mask) {
        return v instanceof Quantity &&
               (((Quantity)v).type & mask)!=0;
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
        public final Value apply() throws ContinuationException {
            switch(id) {
            case VECTOR: return new SchemeVector(ZV);
            case VALUES: return new Values(ZV);
            case APPEND: return EMPTYLIST;
            default:
                throwArgSizeException();            
            }
            return VOID;
        }

        public final Value apply(Value v1) throws ContinuationException {
            switch(id) {        
            case MIN: case MAX: return num(v1);
            case VECTOR: return new SchemeVector(new Value[] {v1});
            case VALUES: return v1;
            case APPEND: return v1;
            case NOT: return truth(v1) ? FALSE : TRUE;
            case CADR:
                return truePair(truePair(v1).cdr()).car();
            case CDAR:
                return truePair(truePair(v1).car()).cdr();
            case CAAR:
                return truePair(truePair(v1).car()).car();
            case CDDR:
                return truePair(truePair(v1).cdr()).cdr();
            case STRINGUPCASE:
                SchemeString str=str(v1);
                return new SchemeString(str.asString().toUpperCase());
            case STRINGDOWNCASE:
                str=str(v1);
                return new SchemeString(str.asString().toLowerCase());
            case MAPCDR:
                Pair lists=pair(v1);
                Pair c=EMPTYLIST;
                while (lists != EMPTYLIST) {
                    c=new Pair(truePair(lists.car()).cdr(), c);
                    lists=pair(lists.cdr());
                }
                return reverseInPlace(c);
            case MAPCAR:
                return mapcar(pair(v1));
            case REVERSE:
                return reverse(pair(v1));
            default:
                throwArgSizeException();            
            }
            return VOID;
        }

        public final Value apply(Value v1, Value v2) throws ContinuationException {
            switch(id) {
            case MAX:
                Quantity q1=num(v1);
                Quantity q2=num(v2);
                Quantity rv=(q1.comp(q2,1) ? q1 : q2);
                if (rv.is(Quantity.EXACT) && (q1.is(Quantity.INEXACT) || q2.is(Quantity.INEXACT)))
                    return rv.toInexact();
                else return rv;
            case MIN: 
                q1=num(v1);
                q2=num(v2);
                rv=(q1.comp(q2,-1) ? q1 : q2);
                if (rv.is(Quantity.EXACT) && (q1.is(Quantity.INEXACT) || q2.is(Quantity.INEXACT)))
                    return rv.toInexact();
                else return rv;
            case CHARLESSTHAN: return truth(character(v1)<character(v2));
            case CHARGRTRTHAN: return truth(character(v1)>character(v2));
            case CHAREQUALCI: return truth(Character.toLowerCase(character(v1))==
                                           Character.toLowerCase(character(v2)));
            case CHARLESSTHANCI: return truth(Character.toLowerCase(character(v1))<
                                              Character.toLowerCase(character(v2)));
            case CHARGRTRTHANCI: return truth(Character.toLowerCase(character(v1))>
                                              Character.toLowerCase(character(v2)));
            case VECTOR: return new SchemeVector(new Value[] {v1, v2});
            case VALUES: return new Values(new Value[] {v1, v2});
            case APPEND: return apply(new Value[] {v1,v2});
            case LISTREF:
                Pair p1=truePair(v1);
                for (int l=num(v2).intValue(); l>0; l--) {
                    p1=truePair(p1.cdr());
                }
                return p1.car();
            case ASSV:
                p1=pair(v2);
                while (p1!=EMPTYLIST) {
                    Pair assc=pair(p1.car());
                    if (assc.car().eqv(v1))
                        return assc;
                    p1=pair(p1.cdr());
                }
                return FALSE;
            case ASSQ:
                return assq(v1, pair(v2));
            case MEMQ:
                return memq(v1, pair(v2));
            case MEMV:
                p1=pair(v2);
                while (p1!=EMPTYLIST) {
                    if (p1.car().eqv(v1))
                        return p1;
                    p1=pair(p1.cdr());
                }
                return FALSE;
            case ASSOC:
                p1=pair(v2);
                while (p1!=EMPTYLIST) {
                    Pair assc=pair(p1.car());
                    if (assc.car().valueEqual(v1))
                        return assc;
                    p1=pair(p1.cdr());
                }
                return FALSE;
            case MEMBER:
                p1=pair(v2);
                while (p1!=EMPTYLIST) {
                    if (p1.car().valueEqual(v1))
                        return p1;
                    p1=pair(p1.cdr());
                }
                return FALSE;
            case STRINGORDER:
                SchemeString str=str(v1);
                SchemeString str2=str(v2);
                return Quantity.valueOf(str.asString().compareTo(str2.asString()));
            case STRINGORDERCI:
                str=str(v1);
                str2=str(v2);
                return Quantity.valueOf(str.asString().compareToIgnoreCase(str2.asString()));
            default:
                throwArgSizeException();            
            }
            return VOID;        
        }

        public final Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            switch(id) {
            case MAX: 
                Quantity q1=num(v1);
                Quantity q2=num(v2);
                Quantity q3=num(v3);
                Quantity rv;
                if (q1.comp(q2,1)) {
                    if (q1.comp(q3,1)) 
                        rv=q1;
                    else 
                        rv=q3;
                } else if (q2.comp(q3,1)) 
                    rv=q2;
                else
                    rv=q3;
                if (rv.is(Quantity.EXACT) && (q1.is(Quantity.INEXACT) || 
                                              q2.is(Quantity.INEXACT) ||
                                              q3.is(Quantity.INEXACT)))
                    return rv.toInexact();
                else return rv;
            case MIN: 
                q1=num(v1);
                q2=num(v2);
                q3=num(v3);
                if (q1.comp(q2,-1)) {
                    if (q1.comp(q3,-1)) 
                        rv=q1;
                    else 
                        rv=q3;
                } else if (q2.comp(q3,-1)) 
                    rv=q2;
                else
                    rv=q3;
                if (rv.is(Quantity.EXACT) && (q1.is(Quantity.INEXACT) || 
                        q2.is(Quantity.INEXACT) ||
                        q3.is(Quantity.INEXACT)))
                    return rv.toInexact();
                else return rv;
            case CHARLESSTHAN: char c2=character(v2);
                return truth(character(v1)<c2 && c2<character(v3));
            case CHARGRTRTHAN: c2=character(v2);
                return truth(character(v1)>c2 && c2>character(v3));
            case CHAREQUALCI: 
                c2=Character.toLowerCase(character(v2));
                return truth(Character.toLowerCase(character(v1))==c2 && c2==
                             Character.toLowerCase(character(v3)));
            case CHARLESSTHANCI: 
                c2=Character.toLowerCase(character(v2));
                return truth(Character.toLowerCase(character(v1))<c2 && c2<
                             Character.toLowerCase(character(v3)));
            case CHARGRTRTHANCI: 
                c2=Character.toLowerCase(character(v2));
                return truth(Character.toLowerCase(character(v1))>c2 && c2>
                             Character.toLowerCase(character(v3)));
            case VECTOR: return new SchemeVector(new Value[] {v1, v2, v3});
            case VALUES: return new Values(new Value[] {v1, v2, v3});
            case APPEND: return apply(new Value[] {v1,v2,v3});
            case SUBSTRING:
                SchemeString str=str(v1);
                int lidx=num(v2).indexValue();
                int uidx=num(v3).indexValue();
                return str.substring(lidx, uidx);
            default:
                throwArgSizeException();            
            }
            return VOID;        
        }

        public final Value apply(Value[] vlr) throws ContinuationException {
            switch(id) {
            case MAX:
                Quantity q1=num(vlr[0]);
                boolean exact=q1.is(Quantity.EXACT);
                for (int i=vlr.length-1; i>0; i--) {
                    Quantity q2=num(vlr[i]);
                    exact = exact && q2.is(Quantity.EXACT);
                    if (q1.comp(q2,-1))
                        q1=q2;
                }
                return (exact ? q1 : q1.toInexact());
            case MIN:
                q1=num(vlr[0]);
                exact=q1.is(Quantity.EXACT);
                for (int i=vlr.length-1; i>0; i--) {
                    Quantity q2=num(vlr[i]);
                    exact = exact && q2.is(Quantity.EXACT);
                    if (q1.comp(q2,1))
                        q1=q2;
                }
                return (exact ? q1 : q1.toInexact());
            case CHARLESSTHAN:
                char c1=character(vlr[0]);
                char c2;
                for (int i=1; i<vlr.length; i++) {
                    c2=character(vlr[i]);
                    if (c1>=c2)
                        return FALSE;
                    c1=c2;
                }
                return TRUE;
            case CHARGRTRTHAN:
                c1=character(vlr[0]);
                for (int i=1; i<vlr.length; i++) {
                    c2=character(vlr[i]);
                    if (c1<=c2)
                        return FALSE;
                    c1=c2;
                }
                return TRUE;
            case CHAREQUALCI:
                char c=Character.toLowerCase(character(vlr[0]));
                for (int i=1; i<vlr.length; i++)
                    if (Character.toLowerCase(character(vlr[i]))!=c)
                        return FALSE;
                return TRUE;
            case CHARLESSTHANCI:
                c1=Character.toLowerCase(character(vlr[0]));
                for (int i=1; i<vlr.length; i++) {
                    c2=Character.toLowerCase(character(vlr[i]));
                    if (c2<=c1)
                        return FALSE;
                    c1=c2;
                }
                return TRUE;
            case CHARGRTRTHANCI:
                c1=Character.toLowerCase(character(vlr[0]));
                for (int i=1; i<vlr.length; i++) {
                    c2=Character.toLowerCase(character(vlr[i]));
                    if (c2>=c1)
                        return FALSE;
                    c1=c2;
                }
                return TRUE;
            case VECTOR:
                int len = vlr.length;
                Value[] newvlr;
                if (len == 0) {
                    newvlr = ZV;
                } else {
                    newvlr = new Value[len];
                    System.arraycopy(vlr,0,newvlr,0,len);
                }
                Value res = new SchemeVector(newvlr);
                return res;
            case VALUES:
                len = vlr.length;
                if (len == 0) {
                    newvlr = ZV;
                } else {
                    newvlr =new Value[len];
                    System.arraycopy(vlr,0,newvlr,0,len);
                }
                res = new Values(newvlr);
                return res;
            case APPEND:
                Pair head_pair=null, tmp_pair, current_pair=null;
                
                int x=0;
                do {
                    Pair working_pair = pair(vlr[x]);
                    while (working_pair != EMPTYLIST) {
                        if (current_pair == null) {
                            head_pair=current_pair=new Pair(working_pair.car(), 
                                                            null);
                        } else {
                            tmp_pair=new Pair(working_pair.car(), EMPTYLIST);
                            current_pair.setCdr(tmp_pair);
                            current_pair=tmp_pair;
                        }
                        working_pair=(Pair)working_pair.cdr();
                    }
                } while (((++x) + 1) < vlr.length);
                if (head_pair == null)
                    return vlr[x];
                else 
                    current_pair.setCdr(vlr[x]);
                return head_pair;
            default:
                throwArgSizeException();
            }
            return VOID;
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

        public final Value apply(Value v1) throws ContinuationException {
            switch(id) {        
            case REVERSEB:
                return reverseInPlace(pair(v1));
            default:
                throwArgSizeException();            
            }
            return VOID;
        }
    }
    
    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {

        public Index() {
            define("append", APPEND);
            define("assq", ASSQ);
            define("assoc", ASSOC);
            define("assv", ASSV);
            define("caar", CAAR);
            define("cadr", CADR);
            define("cdar", CDAR);
            define("cddr", CDDR);
            define("max", MAX);
            define("memq", MEMQ);
            define("member", MEMBER);
            define("min", MIN);
            define("list-ref", LISTREF);
            define("memv", MEMV);
            define("not", NOT);
            define("reverse", REVERSE);
            define("reverse!", Complex.class, REVERSEB);
            define("char<?", CHARLESSTHAN);
            define("char-ci>?", CHARGRTRTHANCI);
            define("char-ci<?", CHARLESSTHANCI);
            define("char-ci=?", CHAREQUALCI);
            define("char>?", CHARGRTRTHAN);
            define("string-order", STRINGORDER);
            define("string-downcase", STRINGDOWNCASE);
            define("string-order-ci", STRINGORDERCI);
            define("string-upcase", STRINGUPCASE);
            define("substring", SUBSTRING);
            define("values", VALUES);
            define("vector", VECTOR);
            define("map-cdr", MAPCDR);
            define("map-car", MAPCAR);
        }
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
        }
        
    }

    //Next: 33
    protected static final int ASSQ = 0,
        MIN = 31,
        MAX = 32,
        MEMQ = 1,
        ASSOC = 6,
        MEMBER = 7,
        CADR = 2,
        CDAR = 3,
        CAAR = 4,
        CDDR = 5,
        NOT = 8,
        APPEND = 9,
        MEMV = 11,
        ASSV = 12,
        VECTOR = 13,
        LISTREF = 14,
        VALUES = 15,
        SUBSTRING = 17,
        CHARGRTRTHAN = 26,
        CHARLESSTHAN = 27,
        CHAREQUALCI = 30,
        CHARGRTRTHANCI = 28,
        CHARLESSTHANCI = 29,
        STRINGORDER = 18,
        STRINGORDERCI = 19,
        STRINGUPCASE = 20,
        STRINGDOWNCASE = 21,
        MAPCDR = 22,
        MAPCAR = 23,
        REVERSE = 24,
        REVERSEB = 25;    
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
