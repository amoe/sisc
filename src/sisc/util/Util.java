package sisc.util;

import java.util.*;
import java.security.AccessControlException;
import java.text.*;
import java.net.*;

import sisc.compiler.*;
import sisc.data.*;
import sisc.env.SymbolicEnvironment;
import sisc.exprs.*;
import sisc.io.*;
import sisc.interpreter.*;
import sisc.nativefun.NativeLibrary;
import java.io.IOException;
import java.io.EOFException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.reflect.Constructor;
import sisc.reader.Lexer;
import sisc.reader.Parser;

public abstract class Util implements Version {

    static String safeGetProperty(String key, String def) {
        try {
            return System.getProperty(key, def);
        } catch (AccessControlException ex) {
            return def;
        }
    }

    public static final boolean caseSensitive =
        safeGetProperty("sisc.caseSensitive",
                        new Boolean(Defaults.CASE_SENSITIVE).toString()).equals("true");
    public static final boolean permitInterrupts =
        safeGetProperty("sisc.permitInterrupts",
                        new Boolean(Defaults.PERMIT_INTERRUPTS).toString()).equals("true");
    public static final int minFloatPrecision =
        Integer.parseInt(safeGetProperty("sisc.minFloatPrecision",
                                         Integer.toString(Defaults.MIN_FLOAT_PRECISION)));
    public static final int maxFloatPrecision =
        Integer.parseInt(safeGetProperty("sisc.maxFloatPrecision",
                                         Integer.toString(Defaults.MAX_FLOAT_PRECISION)));

    public static final Value[] ZV = new Value[0];
    public static final Quantity FIVE = Quantity.valueOf(5);

    public static EOFObject EOF = EOFObject.EOF;
    public static Syntax QUOTE = 
        new Syntax(sisc.compiler.Compiler.QUOTE);
    public static EmptyList EMPTYLIST = EmptyList.EMPTYLIST;
    public static SchemeVoid VOID = SchemeVoid.VOID;
    public static SchemeBoolean TRUE = SchemeBoolean.TRUE,
        FALSE = SchemeBoolean.FALSE;
    public static SchemeVector EMPTYVEC = 
        new SchemeVector(new Value[]{});

    public static Symbol BEGIN = Symbol.get("begin"),
        ERRORK = Symbol.get("error-continuation"),
        EXPSC = Symbol.get("*sc-expander*"),
        EXPTOP = Symbol.get("*top*"),
        FCONT = Symbol.get("failure-continuation"),
        JEXCEPTION = Symbol.get("java-exception"),
        LAMBDA = Symbol.get("lambda"),
        LOCATION = Symbol.get("location"),
        MESSAGE = Symbol.get("message"),
        NAME = Symbol.get("name"),
        OTHER = Symbol.get("other"),
        PARENT = Symbol.get("parent"),
        QUOTESYM = Symbol.get("quote"),
        REPORT = Symbol.get("*report*"),
        SETBANG = Symbol.get("set!"),
        SISC = Symbol.get("*sisc*"),
        SISC_SPECIFIC = Symbol.get("*sisc-specific*"),
        SYMENV = Symbol.get("*symenv*"),
        THIS = Symbol.get("this"),
        TOPLEVEL = Symbol.get("*toplevel*"),
        BACKQUOTE = Symbol.get("quasiquote"),
        UNQUOTE = Symbol.get("unquote"),
        UNQUOTE_SPLICING = Symbol.get("unquote-splicing"),
        SOURCE_LINE = Symbol.get("line-number"),
        SOURCE_COLUMN = Symbol.get("column-number"),
        SOURCE_FILE = Symbol.get("source-file"),
        EVAL = Symbol.get("eval");

    public static String warn(String messageClass) {
        StringBuffer b = new StringBuffer("{");
        b.append(liMessage(SISCB, "warning"));
        b.append(": ");
        b.append(liMessage(SISCB, messageClass));
        b.append(')');
        return b.toString();
    }

    public static String warn(String messageClass, String sourceFile, 
                              int lineNumber, int columnNumber) {
        StringBuffer b = new StringBuffer("{");
        b.append(liMessage(SISCB, "warning"));
        b.append(": ");
        b.append(liMessage(SISCB, messageClass));
        b.append("\n ");
        b.append(sourceFile).append(':').append(lineNumber);
        b.append(':').append(columnNumber).append(": }");
        return b.toString();
    }

    public static String warn(String messageClass, String arg) {
        StringBuffer b = new StringBuffer("{");
        b.append(liMessage(SISCB, "warning"));
        b.append(": ");
        b.append(liMessage(SISCB, messageClass, arg));
        b.append(')');
        return b.toString();
    }

    static Class JOBJ = null;
    static Class[] OBJARRY = new Class[] { Object.class };
    static Constructor JOBJCONST = null;
    static {
        try {
            JOBJ = Class.forName("sisc.modules.s2j.JavaObject");
            JOBJCONST = JOBJ.getConstructor(OBJARRY);
        } catch (Exception cnf) {
        }
    }

    public static Value javaWrap(Object o) {
        if (JOBJ != null)
            try {
                return (Value) JOBJCONST.newInstance(new Object[] { o });
            } catch (Exception ie) {
            }
        return FALSE;
    }

    public static void error(Interpreter r,
                             Value where,
                             String errormessage,
                             Pair moreData)
        throws ContinuationException {
        error(r, append(moreData, list(new Pair(MESSAGE, new SchemeString(errormessage)),
                                       new Pair(LOCATION, where))));
    }

    public static void error(
                             Interpreter r,
                             Value where,
                             String errormessage,
                             Exception e)
        throws ContinuationException {
        error(r, list(new Pair(MESSAGE, new SchemeString(errormessage)),
                      new Pair(LOCATION, where),
                      new Pair(JEXCEPTION, javaWrap(e))));
    }

    public static void error(Interpreter r, Value where, String errormessage)
        throws ContinuationException {
        error(r, list(new Pair(MESSAGE, new SchemeString(errormessage)),
                      new Pair(LOCATION, where)));
    }

    public static void error(Interpreter r, String errormessage, Pair moreData)
        throws ContinuationException {
        error(r, new Pair(new Pair(MESSAGE, new SchemeString(errormessage)),
                          moreData));
    }

    public static void error(Interpreter r, String errormessage)
        throws ContinuationException {
        error(r, list(new Pair(MESSAGE, new SchemeString(errormessage))));
    }

    public static void error(Interpreter r, Value errormessage)
        throws ContinuationException {
        error(r, list(new Pair(MESSAGE, errormessage)));
    }

    public static String simpleErrorToString(Pair p) {
        StringBuffer b=new StringBuffer();
        String location=null;
        String message=null;
        Pair parent = null;
        while (p!=EMPTYLIST && (location==null || message==null)) {
            Pair cp=(Pair)p.car();
            if (cp.car().equals(MESSAGE))
                message=cp.cdr().toString();
            else if (cp.car().equals(LOCATION))
                location=cp.cdr().toString();
            else if (cp.car().equals(PARENT))
                parent=(Pair)cp.cdr();
            p=(Pair)p.cdr();
        }
        if (location==null)
            b.append(liMessage(SISCB, "error"));
        else 
            b.append(liMessage(SISCB, "errorinwhere", location));
        if (message!=null) 
            b.append(": ").append(message);
        else
            b.append('.');
        if (parent!=null)
            b.append("\n  ").append(simpleErrorToString(parent));
        return b.toString();
    }

    public static ClassLoader currentClassLoader() {
        ClassLoader cl = null;
        try {
            cl = Thread.currentThread().getContextClassLoader();
        } catch (java.security.AccessControlException e) {
        }
        if (cl == null) {
            try {
                cl = Util.class.getClassLoader();
            } catch (java.security.AccessControlException e) {
            }
        }
        if (cl == null) {
            try {
                cl = ClassLoader.getSystemClassLoader();
            } catch (java.security.AccessControlException e) {
            }
        }
        if (cl == null) {
            throw new RuntimeException(liMessage(SISCB, "notclassloader"));
        }
        return cl;
    }

    public static Value read(String expr) throws IOException {
        PushbackReader ip = new PushbackReader(new StringReader(expr));
        Parser p = new Parser(new Lexer());
        Value res = p.nextExpression(ip);
        try {
            Value v=p.nextExpression(ip);
        } catch (EOFException eof) {
            return res;
        }
        throw new IOException(liMessage(SISCB, "stringreaderror", expr));
    }

    public static void error(Interpreter r, Pair error)
        throws ContinuationException {
        r.error(error);
    }

    public static String justify(String v, int p, char c) {
        StringBuffer b = new StringBuffer();
        while (b.length() < (p - v.length())) {
            b.append(c);
        }
        return b.append(v).toString();
    }

    public static final void argCheck(Pair argl, int arity) throws Exception {
        int x = length(argl);
        if (x != arity && arity != -1) {
            throw new RuntimeException(liMessage(SISCB,
                                                 "notenoughargs",
                                                 new Object[] { new Integer(arity), new Integer(x)}));
       }
    }

    public static void updateName(Value v, Symbol s) {
        if (v instanceof NamedValue) {
            NamedValue nv = (NamedValue) v;
            if (nv.getName() == null) {
                nv.setName(s);
            }
        }
    }

    public static int length(Pair p) {
        Pair s = p;
        try {
            int i = 0;
            for (; p != EMPTYLIST; i++) {
                p = (Pair) p.cdr();
            }
            return i;
        } catch (ClassCastException ce) {
            throw new RuntimeException(liMessage(SISCB, "notaproperlist", s.synopsis()));
        }
    }

    /**
     * @param p  the head of a list
     * @return   a Vector containing the same elements as the list
     * 
     * @deprecated Obsoleted by pairToExpressions and pairToValues.
     */
    public static Vector pairToExpVect(Pair p) {
        Vector v = new Vector();
        for (; p != EMPTYLIST; p = (Pair) p.cdr()) {
            v.addElement(p.car());
        }

        return v;
    }
    
    public static Expression[] pairToExpressions(Pair p) {
        int           len = length(p);
        Expression[] es  = new Expression[len];

        for (int i = 0; i < len; ++i) {
            es[i] = p.car();
            p     = (Pair)p.cdr();
        }
        
        return es;
    }

    public static Value[] pairToValues(Pair p) {
        int      len = length(p);
        if (len == 0) return ZV;

        Value[] vs  = new Value[len];
        for (int i = 0; i < len; ++i) {
            vs[i] = p.car();
            p     = (Pair)p.cdr();
        }
        
        return vs;
    }

    public static Symbol[] argsToSymbols(Pair p) {
        if (p == EMPTYLIST) {
            return new Symbol[0];
        }

        // Count the proper elements ignoring the tail: 
        int    l = 1;
        Value q = p.cdr();
        while ((q instanceof Pair) && (q != EMPTYLIST)) {
            ++l;
            q = ((Pair)q).cdr();
        }

        // Allocate result array:
        Symbol[] result;

        if (q == EMPTYLIST) {
            result = new Symbol[l];
        } else {
            // improper list: the tail is expected to contain a symbol
            result = new Symbol[l+1];
            result[l] = (Symbol)q;
        }

        // Copy the proper elements into the result
        int i = 0;
        for (;;) {
            result[i++] = (Symbol)p.car();

            if (i == l) {
                // An update of p as done below would throw a
                // ClassCastException for improper lists.
                break;
            }

            p = (Pair)p.cdr();
        }
        
        return result;
    }

    /* Casting checks */
    public static void typeError(String type, Value o) {
        typeError(SISCB, type, o);
    }

    public static void typeError(Symbol bundleName, String type, Value o) {
        if (o instanceof Values)
            throw new RuntimeException(liMessage(SISCB, "multiplevalues"));
        throw new RuntimeException(liMessage(SISCB,
                                             "unexpectedarg",
                                             liMessage(bundleName, type),
                                             o.synopsis()));
    }

    public static final Pair truePair(Value o) {
        if (o == EMPTYLIST)
            typeError("pair", o);
        return (Pair) o;
    }

    /* IO Type casts */
    
    public static final OutputPort outport(Value o) {
        if (o instanceof OutputPort) {
            return (OutputPort) o;
        } else {
            typeError("output-port", o);
            return null;
        }
    }

    public static final SchemeBinaryOutputPort binoutport(Value o) {
        if (o instanceof SchemeBinaryOutputPort) {
            return (SchemeBinaryOutputPort) o;
        } else {
            typeError("binary-output-port", o);
            return null;
        }
    }

    public static final OutputStream binoutstream(Value o) {
        return binoutport(o).getOutputStream();
    }   

    public static final SchemeCharacterOutputPort charoutport(Value o) {
        if (o instanceof SchemeCharacterOutputPort) {
            return (SchemeCharacterOutputPort) o;
        } else {
            typeError("character-output-port", o);
            return null;
        }
    }

    public static final Writer charoutwriter(Value o) {
        return charoutport(o).getWriter();
    }

    public static final InputPort inport(Value o) {
        if (o instanceof InputPort) {
            return (InputPort) o;
        } else {
            typeError("input-port", o);
            return null;
        }
    }
        
    public static final SchemeBinaryInputPort bininport(Value o) {
        if (o instanceof SchemeBinaryInputPort) {
            return (SchemeBinaryInputPort) o;
        } else {
            typeError("binary-input-port", o);
            return null;
        }
    }

    public static final InputStream bininstream(Value o) {
        return bininport(o).getInputStream();
    }
    
    public static final SchemeCharacterInputPort charinport(Value o) {
        if (o instanceof SchemeCharacterInputPort) {
            return (SchemeCharacterInputPort) o;
        } else {
            typeError("character-input-port", o);
            return null;
        }
    }

    public static final Reader charinreader(Value o) {
        return charinport(o).getReader();
    }

    public static final SymbolicEnvironment env(Value o) {
        if (o instanceof SymbolicEnvironment) {
            return (sisc.env.SymbolicEnvironment) o;
        } else {
            typeError("environment", o);
            return null;
        }
    }

    public static final Box box(Value o) {
        if (o instanceof Box) {
            return (Box) o;
        } else {
            typeError("box", o);
            return null;            
        }
    }

    public static final CallFrame cont(Value o) {
        if (o instanceof CallFrame) {
            return (CallFrame) o;
        } else {
            typeError("continuation", o);
            return null;
        }
    }

    public static final Expression expr(Value o) {
        if (o instanceof ExpressionValue) {
            return ((ExpressionValue)o).e;
        } else {
            typeError("expression", o);
            return null;
        }
    }

    public static final AnnotatedExpr annotated(Value o) {
        if (o instanceof AnnotatedExpr) {
            return (AnnotatedExpr) o;
        } else {
            typeError("annotatedexpression", o);
            return null;
        }
    }

    public static URL makeURL(String url) {
        URL res = null;
        if (url == null) return res;
        try {
            res = new URL(url);
        } catch (MalformedURLException e) {
            try {
                res = new URL("file", null, url);
            } catch (MalformedURLException ee) {
            }
        }
        return res;
    }

    public static URL url(Value v) {
        try {
            return url(SchemeString.asString(v));
        } catch (MalformedURLException e) {
            typeError("url", v);
        }
        return null;
    }

    public static URL url(String s) throws MalformedURLException {
        try {
            return new URL(s);
        } catch (MalformedURLException e) {
            return new URL("file", null, s);
        }
    }

    public static URL url(Value current, Value v) {
        URL c = url(current);
        String s = SchemeString.asString(v);
        try {
            return new URL(c, s);
        } catch (MalformedURLException e) {
            try {
                return new URL(c, "file:" + s);
            } catch (MalformedURLException ee) {
                typeError("url", v);
            }
            return null;
        }
    }

    public static final NativeLibrary nlib(Value o) {
        if (o instanceof NativeLibrary) {
            return (NativeLibrary) o;
        } else {
            typeError("nativelibrary", o);
            return null;
        }        
    }

    public static final ImmutablePair immutablePair(Value o) {
        if (o instanceof ImmutablePair) {
            return (ImmutablePair) o;
        } else {
            typeError("immutable-pair", o);
            return null;
        }
    }

    public static final ImmutableVector immutableVector(Value o) {
        if (o instanceof ImmutableVector) {
            return (ImmutableVector) o;
        } else {
            typeError("immutable-vector", o);
            return null;
        }
    }

    public static final SchemeBoolean truth(boolean b) {
        return b ? TRUE : FALSE;
    }

    public static final boolean truth(Value v) {
        return v != FALSE;
    }

    /* List functions */
    public static Value assq(Value v, Pair p) {
        while (p!=EMPTYLIST) {
            Pair assc = (Pair) p.car();
            if (assc.car() == v) {
                return assc;
            }
            p = (Pair) p.cdr();
        }
        return FALSE;
    }

    public static Pair mapcar(Pair list) {
        Pair c=EMPTYLIST;
        while (list != EMPTYLIST) {
            c=new Pair(truePair(list.car()).car(), c);
            list=(Pair) list.cdr();
        }
        return reverseInPlace(c);
    }

    public static Pair reverse(Pair p) {
        Pair n=EMPTYLIST;
        while (p!=EMPTYLIST) {
            n=new Pair(p.car(), n);
            p=(Pair)p.cdr();
        }
        return n;
    }

    public static Pair reverseInPlace(Pair s) {
        if (s==EMPTYLIST) {
            return EMPTYLIST;
        }
        Pair r=EMPTYLIST;
        Value d;
        for (;;) {
            d=s.cdr();
            s.setCdr(r);
            r=s;
            if (d==EMPTYLIST) {
                break;
            }
            s=(Pair)d;
        }
        return r;
    }

    public static Pair append(Pair p1, Pair p2) {
        if (p1 == EMPTYLIST)
            return p2;
        return new Pair(p1.car(), append((Pair) p1.cdr(), p2));
    }

    public static final Pair list(Value o1) {
        return new Pair(o1, EMPTYLIST);
    }

    public static final Pair list(Value o1, Value o2) {
        return new Pair(o1, list(o2));
    }

    public static final Pair list(Value o1, Value o2, Value o3) {
        return new Pair(o1, list(o2, o3));
    }

    public static final Pair list(Value o1, Value o2, Value o3, Value o4) {
        return new Pair(o1, list(o2, o3, o4));
    }

    public static final Pair list(Value o1, Value o2, Value o3, Value o4, Value o5) {
        return new Pair(o1, list(o2, o3, o4, o5));
    }

    public static final Pair valArrayToList(Value[] r, int offset, int len) {
        Pair p = EMPTYLIST;
        for (int i = (offset + len) - 1; i >= offset; i--) {
            p = new Pair(r[i], p);
        }
        return p;
    }

    public static final Pair valArrayToList(Value[] r) {
        return (r == null ? EMPTYLIST : valArrayToList(r, 0, r.length));
    }

    public static final SchemeVector valArrayToVec(Value[] r) {
        if (r == null) return EMPTYVEC;
        //replace nulls with VOID - this mutation is always safe
        for (int i = 0; i < r.length; i++) {
            if (r[i] == null) r[i] = VOID;
        }
        return new SchemeVector(r);
    }

    public static Value memq(Value v, Pair p) {
        while (p!=EMPTYLIST) {
            if (p.car() == v) {
                return p;
            }
            p = (Pair) p.cdr();
        }
        return FALSE;
    }

    /* Localization and Internationalization */
    public static Symbol SISCB = Symbol.intern("sisc.Messages");
    public static WeakHashMap bundles = new WeakHashMap();
    static Locale myLocale = Locale.getDefault();
    static MessageFormat formatter = new MessageFormat("");

    static {
        formatter.setLocale(myLocale);
    }

    public static void registerBundle(Symbol bundleName)
        throws MissingResourceException {
        ResourceBundle b = ResourceBundle.getBundle(bundleName.symval);
        bundles.put(bundleName, b);
    }

    public static String liMessage(Symbol bundleName, String messageName) {
        ResourceBundle bundle = (ResourceBundle) bundles.get(bundleName);
        try {
            if (bundle == null) {
                registerBundle(bundleName);
                bundle = (ResourceBundle) bundles.get(bundleName);
            }
            return bundle.getString(messageName);
        } catch (MissingResourceException mr) {
            if (!bundleName.equals(SISCB))
                return liMessage(SISCB, messageName);
            else
                return "<localized message not found: " + messageName + ">";
        }
    }

    public static String liMessage(Symbol bundle,
                                   String messageName,
                                   String arg1) {
        return MessageFormat.format(liMessage(bundle, messageName),
                                    new Object[] { arg1 });
    }

    public static String liMessage(Symbol bundle,
                                   String messageName,
                                   String arg1,
                                   String arg2) {
        return MessageFormat.format(liMessage(bundle, messageName),
                                    new Object[] { arg1, arg2 });
    }

    public static String liMessage(Symbol bundle,
                                   String messageName,
                                   String arg1,
                                   String arg2,
                                   String arg3) {
        return MessageFormat.format(liMessage(bundle, messageName),
                                    new Object[] { arg1, arg2, arg3 });
    }

    public static String liMessage(Symbol bundle,
                                   String messageName,
                                   String arg1,
                                   String arg2,
                                   String arg3,
                                   String arg4) {
        return MessageFormat.format(liMessage(bundle, messageName),
                                    new Object[] { arg1, arg2, arg3, arg4 });
    }

    public static String liMessage(Symbol bundle,
                                   String messageName,
                                   String arg1,
                                   int arg2,
                                   int arg3) {
        return MessageFormat.format(liMessage(bundle, messageName),
                                    new Object[] { arg1, new Integer(arg2), new Integer(arg3)});
    }

    public static String liMessage(Symbol bundle,
                                   String messageName,
                                   Object[] args) {
        return MessageFormat.format(liMessage(bundle, messageName), args);
    }

    protected static String javaExceptionToString(Exception e) {
        return "<" + e.getClass().getName() + ">: "+ e.getMessage();
    }

    public static Pair sourceAnnotations(String file,
                                         int line,
                                         int column,
                                         Pair anns) {
        return
            new Pair(new Pair(SOURCE_FILE, new SchemeString(file)),
                     new Pair(new Pair(SOURCE_LINE, Quantity.valueOf(line)),
                              new Pair(new Pair(SOURCE_COLUMN, Quantity.valueOf(column)),
                                       anns)));
    }

    public static Expression annotatedAppEval(Class clazz, String fn) {
        Expression e = new AppEval();
        e.setAnnotation(SOURCE_FILE,
                        new SchemeString(clazz.getName() + "/" + fn));
        return e;
    }

    private static Charset defaultCharset = null;

    static {
        try {
            defaultCharset = Charset.forName("UTF-8");
        } catch (UnsupportedEncodingException e) {
            // I think this is a "can't happen" error,
            // since Java natively supports UTF-8
            throw new ExceptionInInitializerError(e);
        } catch (java.nio.charset.UnsupportedCharsetException e) {
            // this one, too
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * Return the default character set, which is UTF-8, but could in
     * principle change.
     *
     * @return a static Charset object
     */
    public static Charset getDefaultCharacterSet() {
        return defaultCharset;
    }

    /**
     * Converts a character set name to a Charset.  If the input name
     * is null, then return the default Charset.  If the input name
     * does not correspond to a legal or supported 
     * character set, then return the default Charset and give a warning.
     *
     * @param charsetName the name of a putative character set, or null
     * @return the Charset corresponding to the argument, or the
     * default character set (see {@link #getDefaultCharacterSet}) if
     * that is not possible
     */
    public static Charset charsetFromString(String charsetName) {
        Charset c;
        if (charsetName == null) {
            c = getDefaultCharacterSet();
        } else {
            try {
                c = Charset.forName(charsetName);
            } catch (Exception e) {
                // This is either IllegalCharsetNameException or
                // UnsupportedCharsetException -- handle both in the same way
                System.err.println(Util.warn("unsupencoding", charsetName));
                c = getDefaultCharacterSet();
            }
        }
        //assert c != null;
        return c;
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
