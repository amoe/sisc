package sisc.modules.s2j;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.io.IOException;

import sisc.data.*;
import sisc.interpreter.Context;
import sisc.interpreter.ContinuationException;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;
import sisc.nativefun.IndexedLibraryAdapter;
import sisc.nativefun.IndexedProcedure;

public class Operation extends IndexedProcedure {

    protected static final int
    //Next: 7
        JAVA_CLASS = 1,
        JAVA_INV_HANDLER = 2,
        JAVA_PROXY_CLASS = 3,
        JAVA_INVOKE_CONSTRUCTOR = 4,
        JAVA_SYNC = 5,
        JAVA_INVOKE_METHOD = 6;


    public static class Index extends IndexedLibraryAdapter {

        public Value construct(Object context, int id) {
            return new Operation(id);
        }

        public Index() {
            define("java/class", JAVA_CLASS);
            define("java/invocation-handler", JAVA_INV_HANDLER);
            define("java/synchronized", JAVA_SYNC);

            define("java/proxy-class", JAVA_PROXY_CLASS);
            define("java/invoke-constructor", JAVA_INVOKE_CONSTRUCTOR);
            define("java/invoke-method", JAVA_INVOKE_METHOD);
        }
    }
    
    private static void processTargetException(Interpreter f,
                                               Throwable e)
        throws ContinuationException {

        if (e instanceof Util.SchemeInvocationException) {
            throwNestedPrimException(liMessage(Util.S2JB, "invocationtargetex"),
                                     ((Util.SchemeInvocationException)e).schemeException);
        } else {
            error(f, Util.makeJObj(e, Throwable.class));
        }
    }

    public Operation(int id) {
        super(id);
    }
    
    public Operation() {}
    
    public Value doApply(Interpreter f) throws ContinuationException {
        SIZESWITCH:
        switch (f.vlr.length) {
        case 1:
            switch(id) {
            case JAVA_CLASS:
                String cname = Symbol.toString(f.vlr[0]);
                try {
                    Class c = Util.resolveType(cname);
                    return Util.makeJObj(c, Class.class);
                } catch(IOException e) {
                    throwPrimException(liMessage(Util.S2JB, "classnotfound", cname));
                }
            case JAVA_INV_HANDLER:
                return Util.makeJObj(new SchemeInvocation(f.dynenv.copy(), (Procedure) f.vlr[0]), SchemeInvocation.class);
            default: break SIZESWITCH;
            }
        case 2:
            switch(id) {
            case JAVA_SYNC:
                synchronized(Util.jobj(f.vlr[0])) {
                    Interpreter i=Context.enter(f.dynenv);
                    try {
                        return i.eval((Procedure) f.vlr[1], ZV);
                    } catch (SchemeException se) {
                        throwNestedPrimException(se);
                    } finally {
                        Context.exit();
                    }
                }
                return VOID;
            case JAVA_INVOKE_CONSTRUCTOR:
                Constructor jc = Util.jconstr(f.vlr[0]);
                try {
                    return Util.makeJObj(invokeConstructor(jc, Util.pairToObjects((Pair) f.vlr[1])),
                                         jc.getDeclaringClass());
                } catch (InvocationTargetException e) {
                    processTargetException(f, e.getTargetException());
                }
            default: break SIZESWITCH;
            }
        case 3:
            switch(id) {
            case JAVA_INVOKE_METHOD:
                Method jm = Util.jmethod(f.vlr[0]);
                try {
                    return Util.makeJObj(invokeMethod(jm, Util.jobj(f.vlr[1]), Util.pairToObjects((Pair) f.vlr[2])),
                                         jm.getReturnType());
                } catch (InvocationTargetException e) {
                    processTargetException(f, e.getTargetException());
                }
            default: break SIZESWITCH;
            }
        default: break SIZESWITCH;
        }
    
        switch(id) {
        case JAVA_PROXY_CLASS:
            Class[] interfaces = new Class[f.vlr.length];
            for (int i=0; i<f.vlr.length; i++) {
                interfaces[i] = Util.jclass(f.vlr[i]);
            }
            try {
                return Util.makeJObj(Proxy.getProxyClass(Util.currentClassLoader(), interfaces), Class.class);
            } catch (IllegalArgumentException e) {
                throw new RuntimeException(liMessage(Util.S2JB, "proxyinterfaceillegal", Reflection.typesString(interfaces)));
            } catch (NullPointerException e) {
                throw new RuntimeException(liMessage(Util.S2JB, "proxyinterfacenull", Reflection.typesString(interfaces)));
            }
        default:
            throwArgSizeException();
        }
        return VOID;
    }

    private static String typesString(Object[] args) {
        StringBuffer res = new StringBuffer();
        for (int i=0; i<args.length; i++) {
            res.append(Reflection.typeString(args[i]));
            if (i<args.length-1) res.append(", ");
        }
        return res.toString();
    }

    private static Object invokeMethod(Method m, Object o, Object[] args)
        throws InvocationTargetException {
    
        try {
            return m.invoke(o, args);
        } catch (NullPointerException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalmethodnull", 
                                                 m.toString(),
                                                 Util.nameType(m.getDeclaringClass())));
        } catch (IllegalAccessException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalmethodaccess", 
                                                 m.toString(), 
                                                 Util.nameType(m.getDeclaringClass())));
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalmethodargument", 
                                                 m.toString(), 
                                                 Util.nameType(m.getDeclaringClass()),
                                                 Reflection.typeString(o),
                                                 typesString(args)));
        }
    }

    private static Object invokeConstructor(Constructor c, Object[] args)
        throws InvocationTargetException {
    
        try {
            return c.newInstance(args);
        } catch (InstantiationException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "constructorabstract",
                                                 c.toString(),
                                                 Util.nameType(c.getDeclaringClass())));
        } catch (IllegalAccessException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalconstructoraccess", 
                                                 c.toString(), 
                                                 Util.nameType(c.getDeclaringClass())));
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(liMessage(Util.S2JB, "illegalconstructorargument",
                                                 c.toString(), 
                                                 Util.nameType(c.getDeclaringClass()),
                                                 typesString(args)));
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
