package sisc.modules.s2j;

import java.lang.reflect.*;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.CommonIndexedProcedure;
import sisc.nativefun.IndexedFixableProcedure;
import sisc.nativefun.IndexedLibraryAdapter;

public class Reflection extends Util {

    protected static final int
    //NEXT = 41
        JAVA_WRAP = 1,
        JAVA_UNWRAP = 2,

        JAVA_CONSTRUCTORS = 3,
        JAVA_METHODS = 4,
        JAVA_FIELDS = 5,
        JAVA_CLASSES = 6,

        JAVA_INTERFACES = 7,
        JAVA_SUPERCLASS = 8,
        JAVA_COMPONENT_TYPE = 9,
        JAVA_INSTANCEQ = 10,
        JAVA_ASSIGNABLEQ = 11,

        JAVA_NAME = 12,
        JAVA_MODIFIERS = 13,
        JAVA_DECLARING_CLASS = 14,
        JAVA_EXCEPTION_TYPES = 15,
        JAVA_PARAMETER_TYPES = 16,
        JAVA_RETURN_TYPE = 17,
        JAVA_FIELD_TYPE = 18,

        JAVA_OBJECTQ = 19,
        JAVA_CLASSQ = 20,
        JAVA_FIELDQ = 21,
        JAVA_METHODQ = 22,
        JAVA_CONSTRUCTORQ = 23,
        JAVA_INTERFACEQ = 24,
        JAVA_PRIMITIVEQ = 25,
        JAVA_ARRAYQ = 26,
        JAVA_ARRAY_CLASSQ = 27,
        JAVA_NULLQ = 28,

        JAVA_CLASS_OF = 29,
        JAVA_ARRAY_CLASS = 30,
        JAVA_ARRAY_NEW = 31,
        JAVA_NULL = 32,
        JAVA_MANGLE_FIELD_NAME = 33,
        JAVA_MANGLE_METHOD_NAME = 34,
        JAVA_MANGLE_CLASS_NAME = 35,

        JAVA_FIELD_REF = 36,
        JAVA_FIELD_SET = 37,
        JAVA_ARRAY_REF = 38,
        JAVA_ARRAY_SET = 39,
        JAVA_ARRAY_LENGTH = 40;


    static String typeString(Object o) {
        return (o == null) ? "null" : Util.nameType(o.getClass());
    }

    private static String typeString(Class o) {
        return (o == null) ? "null" : Util.nameType(o);
    }

    static String typesString(Class[] args) {
        StringBuffer res = new StringBuffer();
        for (int i=0; i<args.length; i++) {
            res.append(typeString(args[i]));
            if (i<args.length-1) res.append(", ");
        }
        return res.toString();
    }
            
    private static Object getField(Field f, Object o) {
        try {
            return f.get(o);
        } catch (NullPointerException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalfieldnull", 
                                                 f.toString(),
                                                 Util.nameType(f.getDeclaringClass())));
        } catch (IllegalAccessException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalfieldaccess",
                                                 f.toString(), 
                                                 Util.nameType(f.getDeclaringClass())));
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalfieldgetargument", 
                                                 f.toString(), 
                                                 Util.nameType(f.getDeclaringClass()),
                                                 typeString(o)));
        }
    }

    private static void setField(Field f, Object o, Object v) {
        try {
            f.set(o, v);
        } catch (NullPointerException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalfieldnull", 
                                                 f.toString(),
                                                 Util.nameType(f.getDeclaringClass())));
        } catch (IllegalAccessException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalfieldaccess", 
                                                 f.toString(), 
                                                 Util.nameType(f.getDeclaringClass())));
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalfieldsetargument", 
                                                 f.toString(), 
                                                 Util.nameType(f.getDeclaringClass()),
                                                 typeString(o),
                                                 typeString(v)));
        }
    }

    private static Object getArray(Object a, int idx) {
        try {
            return Array.get(a, idx);
        } catch (NullPointerException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalarraynull"));
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "arraybounds", String.valueOf(idx)));
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "notarray", typeString(a)));
        }
    }

    private static void setArray(Object a, int idx, Object v) {
        try {
            Array.set(a, idx, v);
        } catch (NullPointerException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalarraynull"));
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "arraybounds", String.valueOf(idx)));
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalarrayargs", typeString(a), typeString(v)));
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

        public Value apply() throws ContinuationException {
            switch(id) {
            case JAVA_NULL:
                return makeJObj(null, Object.class);
            default: throwArgSizeException();
            }
            return VOID;
        }
        
        public Value apply(Value v1) throws ContinuationException {
            switch(id) {
            case JAVA_WRAP:
                return makeJObj(v1, Value.class);
            case JAVA_UNWRAP:
                return (Value)jobj(v1);
            case JAVA_CONSTRUCTORS:
                try {
                    return objectsToList(jclass(v1).getDeclaredConstructors());
                } catch (SecurityException e) { return FALSE; }
            case JAVA_METHODS:
                try {
                    return objectsToList(jclass(v1).getDeclaredMethods());
                } catch (SecurityException e) { return FALSE; }
            case JAVA_FIELDS:
                try {
                    return objectsToList(jclass(v1).getDeclaredFields());
                } catch (SecurityException e) { return FALSE; }
            case JAVA_CLASSES:
                try {
                    return objectsToList(jclass(v1).getDeclaredClasses());
                } catch (SecurityException e) { return FALSE; }
            case JAVA_INTERFACES:
                return objectsToList(jclass(v1).getInterfaces());
            case JAVA_SUPERCLASS:
                return makeJObj(jclass(v1).getSuperclass(), Class.class);
            case JAVA_COMPONENT_TYPE:
                return makeJObj(jclass(v1).getComponentType(), Class.class);
            case JAVA_NAME:
                switch (jtype(v1)) {
                case JavaObject.JCLASS:
                    return Symbol.intern(Util.nameType(jclass(v1)));
                default:
                    return Symbol.intern(((Member)jobj(v1)).getName());
                }
            case JAVA_MODIFIERS:
                int mods = 0;
                switch (jtype(v1)) {
                case JavaObject.JCLASS:
                    mods = jclass(v1).getModifiers();
                    break;
                default:
                    mods = ((Member)jobj(v1)).getModifiers();
                    break;
                }
                String smods = Modifier.toString(mods);
                Pair p = EMPTYLIST;
                if (smods.length() == 0) return p;
                for(int idx = smods.length(), nidx=0; nidx>=0; idx = nidx) {
                    nidx = smods.lastIndexOf(' ', idx-1);
                    p = new Pair(Symbol.intern(smods.substring(nidx+1, idx)), p);
                }
                return p;
            case JAVA_DECLARING_CLASS:
                switch (jtype(v1)) {
                case JavaObject.JCLASS:
                    return makeJObj(jclass(v1).getDeclaringClass(), Class.class);
                default:
                    return makeJObj(((Member)jobj(v1)).getDeclaringClass(), Class.class);
                }
            case JAVA_EXCEPTION_TYPES:
                switch (jtype(v1)) {
                case JavaObject.JCONSTR:
                    return objectsToList(jconstr(v1).getExceptionTypes());
                case JavaObject.JMETHOD:
                default:
                    return objectsToList(jmethod(v1).getExceptionTypes());
                }
            case JAVA_PARAMETER_TYPES:
                switch (jtype(v1)) {
                case JavaObject.JCONSTR:
                    return objectsToList(jconstr(v1).getParameterTypes());
                case JavaObject.JMETHOD:
                default:
                    return objectsToList(jmethod(v1).getParameterTypes());
                }
            case JAVA_RETURN_TYPE:
                switch (jtype(v1)) {
                case JavaObject.JCONSTR:
                    return makeJObj(jclass(v1).getDeclaringClass(), Class.class);
                case JavaObject.JMETHOD:
                default:
                    return makeJObj(jmethod(v1).getReturnType(), Class.class);
                }
            case JAVA_FIELD_TYPE:
                return makeJObj(jfield(v1).getType(), Class.class);
            case JAVA_OBJECTQ:
                return truth(v1 instanceof JavaObject);
            case JAVA_CLASSQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JCLASS);
            case JAVA_FIELDQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JFIELD);
            case JAVA_METHODQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JMETHOD);
            case JAVA_CONSTRUCTORQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JCONSTR);
            case JAVA_ARRAYQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JARRAY);
            case JAVA_NULLQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JNULL);
            case JAVA_INTERFACEQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JCLASS && jclass(v1).isInterface());
            case JAVA_PRIMITIVEQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JCLASS && jclass(v1).isPrimitive());
            case JAVA_ARRAY_CLASSQ:
                return truth(v1 instanceof JavaObject && jtype(v1) == JavaObject.JCLASS && jclass(v1).isArray());
            case JAVA_CLASS_OF:
                return makeJObj(sjobj(v1).classOf(), Class.class);
            case JAVA_NULL:
                return makeJObj(null, jclass(v1));
            case JAVA_MANGLE_FIELD_NAME:
                return Symbol.intern(mangleFieldName(symval(v1)));
            case JAVA_MANGLE_METHOD_NAME:
                return Symbol.intern(mangleMethodName(symval(v1)));
            case JAVA_MANGLE_CLASS_NAME:
                return Symbol.intern(mangleClassName(symval(v1)));
            case JAVA_ARRAY_LENGTH:
                Object ar = jobj(v1);
                try {
                    return Quantity.valueOf(Array.getLength(ar));
                } catch (IllegalArgumentException e) {
                    throw new RuntimeException(liMessage(Util.S2JB, "notarray", typeString(ar)));
                }
            default: throwArgSizeException();
            }
            return VOID;
        }
        
        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch(id) {
            case JAVA_INSTANCEQ:
                return truth(((v2 instanceof JavaPrimitive) ?
                              fixClass(jclass(v1)) :
                              jclass(v1)).isInstance(jobj(v2)));
            case JAVA_ASSIGNABLEQ:
                Class c1 = jclass(v2);
                Class c1Fixed = fixClass(c1);
                return truth(((c1 != c1Fixed) ?
                              fixClass(jclass(v1)) :
                              jclass(v1)).isAssignableFrom(c1Fixed));
            case JAVA_ARRAY_CLASS:
                try {
                    return makeJObj(makeArrayClass(jclass(v1), ((Quantity) v2).indexValue()), Class.class);
                } catch (NullPointerException e) {
                    throw new RuntimeException(liMessage(S2JB, "arraytypenull"));
                } catch (IllegalArgumentException e) {
                    throw new RuntimeException(liMessage(S2JB, "arraytypevoid"));
                } catch (NegativeArraySizeException e) {
                    throw new RuntimeException(liMessage(S2JB, "arraynegative", v2.toString()));
                }
            case JAVA_ARRAY_NEW:
                Value dims = v2;
                Value[] dimensions;
                if (dims instanceof Pair) {
                    dimensions = pairToValues(pair(dims));
                } else if (dims instanceof SchemeVector) {
                    dimensions = vec(dims).vals;
                } else {
                    dimensions = new Value[]{dims};
                }
                int[] intDims = new int[dimensions.length];
                for (int i=0; i< dimensions.length; i++) {
                    intDims[i] = ((Quantity) dimensions[i]).indexValue();
                }
                Class componentType = jclass(v1);
                try {
                    return makeJObj(Array.newInstance(componentType, intDims),
                                    makeArrayClass(componentType, intDims.length));
                } catch (NullPointerException e) {
                    throw new RuntimeException(liMessage(S2JB, "arraytypenull"));
                } catch (IllegalArgumentException e) {
                    throw new RuntimeException(liMessage(S2JB, "arraytypevoid"));
                } catch (NegativeArraySizeException e) {
                    throw new RuntimeException(liMessage(S2JB, "arraynegative", dims.toString()));
                }
            case JAVA_FIELD_REF:
                Field jf = jfield(v1);
                return Util.makeJObj(getField(jf, jobj(v2)), jf.getType());
            case JAVA_ARRAY_REF:
                Object ar = jobj(v1);
                return Util.makeJObj(getArray(ar, ((Quantity) v2).indexValue()), ar.getClass().getComponentType());
            default: throwArgSizeException();
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

        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            switch(id) {
            case JAVA_FIELD_SET:
                setField(jfield(v1), jobj(v2), jobj(v3));
                return VOID;
            case JAVA_ARRAY_SET:
                setArray(jobj(v1), ((Quantity) v2).indexValue(), jobj(v3));
                return VOID;
            default: throwArgSizeException();
            }
            return VOID;
        }
    }
    
    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {

        public Index() {
            define("java/wrap", JAVA_WRAP);
            define("java/unwrap", JAVA_UNWRAP);

            define("java/constructors", JAVA_CONSTRUCTORS);
            define("java/methods", JAVA_METHODS);
            define("java/fields", JAVA_FIELDS);
            define("java/classes", JAVA_CLASSES);

            define("java/interfaces", JAVA_INTERFACES);
            define("java/superclass", JAVA_SUPERCLASS);
            define("java/component-type", JAVA_COMPONENT_TYPE);
            define("java/instance?", JAVA_INSTANCEQ);
            define("java/assignable?", JAVA_ASSIGNABLEQ);

            define("java/name", JAVA_NAME);
            define("java/modifiers", JAVA_MODIFIERS);
            define("java/declaring-class", JAVA_DECLARING_CLASS);
            define("java/exception-types", JAVA_EXCEPTION_TYPES);
            define("java/parameter-types", JAVA_PARAMETER_TYPES);
            define("java/return-type", JAVA_RETURN_TYPE);
            define("java/field-type", JAVA_FIELD_TYPE);

            define("java/object?", JAVA_OBJECTQ);
            define("java/class?", JAVA_CLASSQ);
            define("java/field?", JAVA_FIELDQ);
            define("java/method?", JAVA_METHODQ);
            define("java/constructor?", JAVA_CONSTRUCTORQ);
            define("java/interface?", JAVA_INTERFACEQ);
            define("java/primitive?", JAVA_PRIMITIVEQ);
            define("java/array?", JAVA_ARRAYQ);
            define("java/array-class?", JAVA_ARRAY_CLASSQ);
            define("java/null?", JAVA_NULLQ);

            define("java/class-of", JAVA_CLASS_OF);
            define("java/array-class", JAVA_ARRAY_CLASS);
            define("java/array-new", JAVA_ARRAY_NEW);
            define("java/null", JAVA_NULL);
            define("java/mangle-field-name", JAVA_MANGLE_FIELD_NAME);
            define("java/mangle-method-name", JAVA_MANGLE_METHOD_NAME);
            define("java/mangle-class-name", JAVA_MANGLE_CLASS_NAME);

            define("java/field-ref", JAVA_FIELD_REF);
            define("java/field-set!", Complex.class, JAVA_FIELD_SET);
            define("java/array-ref", JAVA_ARRAY_REF);
            define("java/array-set!", Complex.class, JAVA_ARRAY_SET);
            define("java/array-length", JAVA_ARRAY_LENGTH);
        }
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
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
