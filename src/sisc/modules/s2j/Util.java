package sisc.modules.s2j;

import java.lang.reflect.*;

import sisc.nativefun.*;
import sisc.interpreter.*;
import sisc.data.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;

public abstract class Util extends IndexedFixableProcedure {

    protected static final Symbol S2JB =
        Symbol.intern("sisc.modules.s2j.Messages");

    public Util() {}
    public Util(int id) {
        super(id);
    }

    public static class SchemeInvocationException extends RuntimeException {

        public SchemeException schemeException;

        public SchemeInvocationException(SchemeException se) {
            super(se.schemeStackTrace());
            this.schemeException = se;
        }
    }

    public static Throwable javaException(SchemeException e) {
        Value message = null;
        for (Pair l = e.m; l != EMPTYLIST; l = (Pair)l.cdr()) {
            if (l.car() instanceof Pair) {
                Pair el = (Pair)l.car();
                if (el.car() == MESSAGE) {
                    message = el.cdr();
                    break;
                }
            }
        }

        if (message != null && message instanceof JavaObject) {
            Object eo = ((JavaObject)message).get();
            if (eo instanceof Throwable)
                return (Throwable)eo;
        }

        return new SchemeInvocationException(e);
    }

    public static final int jtype(Value o) {
        try {
            return ((JavaObject)o).getObjType();
        } catch (ClassCastException e) { typeError(S2JB, "jobject", o); }
        return JavaObject.JUNKN;
    }

    public static final JavaObject sjobj(Value o) {
        try {
            return (JavaObject)o;
        } catch (ClassCastException e) { typeError(S2JB, "jobject", o); }
        return null;
    }

    public static final Object jobj(Value o) {
        try {
            return ((JavaObject)o).get();
        } catch (ClassCastException e) { typeError(S2JB, "jobject", o); }
        return null;
    }

    public static final Class jclass(Value o) {
        try {
            return (Class)((JavaObject)o).get();
        } catch (ClassCastException e) { typeError(S2JB, "jclass", o); }
        return null;
    }

    public static final Constructor jconstr(Value o) {
        try {
            return (Constructor)((JavaObject)o).get();
        } catch (ClassCastException e) { typeError(S2JB, "jconstructor", o); }
        return null;
    }

    public static final Method jmethod(Value o) {
        try {
            return (Method)((JavaObject)o).get();
        } catch (ClassCastException e) { typeError(S2JB, "jmethod", o); }
        return null;
    }

    public static final Field jfield(Value o) {
        try {
            return (Field)((JavaObject)o).get();
        } catch (ClassCastException e) { typeError(S2JB, "jfield", o); }
        return null;
    }

    public static final JavaObject makeJObj(Object o) {
        if (o == null) throw new RuntimeException(liMessage(S2JB, "unexpectednull"));
        return new JavaObject(o);
    }

    public static final JavaObject makeJObj(Object o, Class c) {
        if (o == null) return new JavaNull(c);
        Class fixedClass = fixClass(c);
        return (fixedClass == c) ?
            new JavaObject(o) :
            new JavaPrimitive(c, o);
    }

    public static final Value objArrayToVec(Object[] objs) {
        Class c = objs.getClass().getComponentType();
        JavaObject[] res = new JavaObject[objs.length];
        for (int i=0; i < objs.length; i++) {
            res[i] = makeJObj(objs[i], c);
        }
        return new SchemeVector(res);
    }

    public static final Pair objectsToList(Object[] objs) {
        Class c = objs.getClass().getComponentType();
        Pair res = EMPTYLIST;
        for (int i=objs.length-1; i >= 0; i--) {
            res = new Pair(makeJObj(objs[i], c), res);
        }
        return res;
    }

    public static Vector pairToObjVect(Pair p) {
        Vector v=new Vector();
        for (; p!=EMPTYLIST; p=(Pair)p.cdr()) {
            v.addElement(jobj(p.car()));
        }
        return v;
    }

    public static final Object[] pairToObjects(Pair p) {
        Vector v = pairToObjVect(p);
        Object[] vs = new Object[v.size()];
        v.copyInto(vs);
        return vs;
    }
    
    /**
     * Construct a class representing an array type for an array with
     * a certain component type and dimensions.
     *
     * The Java reflection API offers no easy way of doing this, so
     * instead we have to create an array instance of the specific
     * type with zero-length dimensions and then get its class.
     *
     * @param c component type of the array type
     * @param dims number of dimensions of the array type 
     * @return array type
     */
    public static Class makeArrayClass(Class c, int dims) {
        return Array.newInstance(c, new int[dims]).getClass();
    }

    private static HashMap primitiveTypesToNames = new HashMap();
    private static HashMap namesToPrimitiveTypes = new HashMap();
    private static HashMap primitiveTypesToClasses = new HashMap();
    private static HashMap classesToPrimitiveTypes = new HashMap();

    static {
        Object[] primitiveTypes = {
            "void",         Void.TYPE,          Void.class,
            "boolean",      Boolean.TYPE,       Boolean.class,
            "char",         Character.TYPE,     Character.class,
            "byte",         Byte.TYPE,          Byte.class,
            "short",        Short.TYPE,         Short.class,
            "int",          Integer.TYPE,       Integer.class,
            "long",         Long.TYPE,          Long.class,
            "float",        Float.TYPE,         Float.class,
            "double",       Double.TYPE,        Double.class
        };

        for (int i=0; i<primitiveTypes.length; i+=3) {
            namesToPrimitiveTypes.put(primitiveTypes[i],
                                      primitiveTypes[i+1]);
            primitiveTypesToNames.put(primitiveTypes[i+1],
                                      primitiveTypes[i]);
            primitiveTypesToClasses.put(primitiveTypes[i+1],
                                        primitiveTypes[i+2]);
            classesToPrimitiveTypes.put(primitiveTypes[i+2],
                                        primitiveTypes[i+1]);
        }
    }
        
    /**
     * Map names of primitive types to their respective classes in
     * the reflection API.
     *
     * @param name primitive type name
     * @return class corresponding to the primitive type, or
     * <code>null</code> if the name was not recognized as that of a
     * primitive type
     */
    public static Class resolvePrimitiveType(String name) {
        return (Class)namesToPrimitiveTypes.get(name);
    }

    /**
     * Map a primitive type to it's name
     *
     * @param c primitive type class
     * @return name corresponding to the primitive type, or
     * <code>null</code> if the class was not recognized as that of a
     * primitive type
     */
    public static String namePrimitiveType(Class c) {
        return (String)primitiveTypesToNames.get(c);
    }

    /**
     * Map a type name to its corresponding class.
     *
     * At the center of this is the Class.forName method. However,
     * that method cannot handle primitive types and array types
     * (actually it can, but one needs to use the VM-internal type
     * coding scheme), so we handle these case separately.
     *
     * @param name type name
     * @param cl classloader to use
     * @return a <code>Class</code> value
     */
    public static Class resolveType(String name, ClassLoader cl)
        throws IOException {

        int idx = name.indexOf('[');
        if (idx != -1) {
            Class c = resolveType(name.substring(0, idx), cl);
            return (c == null) ?
                null : makeArrayClass(c, (name.length()-idx)/2);
        }
        Class res = resolvePrimitiveType(name);
        if (res != null) return res;
        try {
            return Class.forName(name, true, cl);
        } catch (ClassNotFoundException e) {
            throw new IOException(e.toString());
        }
    }

    /**
     * Map a type name to its corresponding class.
     *
     * This just calls <code>resolveType(name,
     * currentClassLoader())</code>.
     */
    public static Class resolveType(String name) throws IOException {
        return resolveType(name, currentClassLoader());
    }

    /**
     * Map a class to its corresponding type name.
     *
     * At the center of this is the Class.getName() method.  However,
     * that method cannot handle primitive types and array types
     * (actually it can, but one needs to use the VM-internal type
     * coding scheme), so we handle these case separately.
     *
     * @param c class
     * @return string naming the type
     */
    public static String nameType(Class c) {
        if (c.isArray()) {
            Class cType = c.getComponentType();
            StringBuffer res = new StringBuffer("[]");
            for(; cType.isArray(); cType=cType.getComponentType()) {
                res.append("[]");
            }
            return nameType(cType)+res;
        }

        String res = namePrimitiveType(c);
        return (res == null) ? c.getName() : res;
    }

    /**
     * Map classes representing primitive types to their corresponding
     * <code>java.lang</code> classes.
     *
     * This is a hack so that we can pretend that primitive types are
     * assignable to their respective <code>java.lang</code> classes
     * and visa versa.
     *
     * @param c class
     * @return if <code>c</code> is a class representing a primitive
     * type then the corresponding <code>java.lang</code> class is
     * returned, otherwise the original class.
     */
    public static Class fixClass(Class c) {
        Class res = (Class)primitiveTypesToClasses.get(c);
        return (res == null) ? c : res;
    }

    public static String mangleFieldName(String s) {
        int l = s.length();
        StringBuffer res = new StringBuffer(l);
        int p = 0;
        char c;
        for (; p<l; p++) {
            c = s.charAt(p);
            if (Character.isJavaIdentifierStart(c)) {
                res.append(c);
                break;
            }
        }
        p++;
        for (; p<l; p++) {
            c = s.charAt(p);
            if (Character.isJavaIdentifierPart(c)) {
                res.append(c);
                continue;
            }
            for (; p<l; p++) {
                c = s.charAt(p);
                if (Character.isJavaIdentifierPart(c)) {
                    res.append(Character.toUpperCase(c));
                    break;
                }
            }
        }
        return res.toString();
    }

    public static String mangleMethodName(String s) {
        int l = s.length();
        if (s.endsWith("!")) {
            s = s.substring(0, l-1);
            l--;
        }
        if (s.endsWith("?")) {
            s = "is-" + s.substring(0, l-1);
            l+=2;
        }

        return mangleFieldName(s);
    }

    public static String mangleClassName(String s) {
        int p;
        int l = s.length();

        //remove angle brackets
        if (s.startsWith("<") && s.endsWith(">")) {
            s = s.substring(1, l-1);
        }

        //convert trailing '*' into []
        StringBuffer tail = new StringBuffer();
        while (s.endsWith("*")) {
            s = s.substring(0, s.length()-1);
            tail.append("[]");
        }

        //preserve trailing []
        while (s.endsWith("[]")) {
            s = s.substring(0, s.length()-2);
            tail.append("[]");
        }

        //perform field mangling on the "between the dots" parts.
        int prev=0;
        StringBuffer res = new StringBuffer(l);
        while((p=s.indexOf('.', prev)) != -1) {
            res.append(mangleFieldName(s.substring(prev, p)))
                .append('.');
            prev = p+1;
        }

        //process '/' as nested class indicator
        String cn;
        while((p=s.indexOf('/', prev)) != -1) {
            cn = mangleFieldName(s.substring(prev, p));
            res.append(Character.toUpperCase(cn.charAt(0)))
                .append(cn.substring(1)).append('$');
            prev = p+1;
        }

        //process trailing class name
        cn = mangleFieldName(s.substring(prev, s.length()));
        res.append(Character.toUpperCase(cn.charAt(0)))
            .append(cn.substring(1));

        //add array markers and return
        return res.append(tail.toString()).toString();
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
