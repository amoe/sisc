package sisc.modules;

import java.util.Iterator;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.data.*;
import sisc.exprs.AnnotatedExpr;

public abstract class Annotations  {
    protected static final int ANNOTATION = 0,
        ANNOTATIONQ = 1,
        ANNOTATIONKEYS = 2,
        ANNOTATIONSRC = 3,
        ANNOTATIONEXPR = 4,
        ANNOTATIONSTRIPPED = 5,
        MAKEANNOTATION = 7,
        SETANNOTATION = 8,
        SETANNOTATIONSTRIPPED = 6;

    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
        }
        
        public Index() {
            define("annotation?", ANNOTATIONQ);
            define("annotation-keys", ANNOTATIONKEYS);
            define("annotation", ANNOTATION);
            define("annotation-source", ANNOTATIONSRC);
            define("annotation-expression", ANNOTATIONEXPR);
            define("annotation-stripped", ANNOTATIONSTRIPPED);
            define("make-annotation", MAKEANNOTATION);
            define("set-annotation!", Complex.class, SETANNOTATION);
            define("set-annotation-stripped!", Complex.class, SETANNOTATIONSTRIPPED);
        }
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
            case ANNOTATIONKEYS: {
                Pair akl=EMPTYLIST;
                for (Iterator i=v1.getAnnotationKeys().iterator(); i.hasNext();) 
                    akl=new Pair((Symbol)i.next(), akl);
                return akl;
            }
            case ANNOTATIONSTRIPPED:
                return annotated(v1).stripped;
            case ANNOTATIONQ:
                return truth(v1 instanceof AnnotatedExpr);
            case ANNOTATIONSRC:
                Value rv;
                if (v1 instanceof AnnotatedExpr) 
                    rv=annotated(v1).annotation;
                else 
                    rv=FALSE;
                return rv;
            case ANNOTATIONEXPR:
                if (v1 instanceof AnnotatedExpr) 
                    return (Value)annotated(v1).expr;
                else return v1;
            default:
                throwArgSizeException();
            }        
            return VOID;
        }
        
        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch (id) {
            case ANNOTATION:
                return v1.getAnnotation((Symbol) v2);
            default:
                throwArgSizeException();
            }
            return VOID;
        }
        
        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            switch (id) {
            case MAKEANNOTATION:
                AnnotatedExpr ae=new AnnotatedExpr(v1, v2);
                ae.stripped=v3;
                return ae;
            case ANNOTATION:
                return v1.getAnnotation((Symbol) v2, v3);
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

        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch (id) {
            case SETANNOTATIONSTRIPPED:
                annotated(v1).stripped=v2;
                return VOID;
            default:
                throwArgSizeException();
            }
            return VOID;
        }

        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            switch (id) {
            case SETANNOTATION:
                return v1.setAnnotation((Symbol) v2, v3);
            default:
                throwArgSizeException();
            }
            return VOID;
        }
        
        public Value apply(Value[] vlr) throws ContinuationException {
            switch(id) {
            case SETANNOTATION:
                return vlr[0].setAnnotation((Symbol) vlr[1],
                                              vlr[2],
                                              vlr[3]);
            default:
                throwArgSizeException();
            }
            return VOID;
        }
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
