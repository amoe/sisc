package sisc.env;

import java.io.*;
import sisc.data.*;
import sisc.io.*;

import java.util.WeakHashMap;
import java.net.URLClassLoader;
import java.net.URL;
import java.security.AccessControlException;
import sisc.interpreter.AppContext;
import sisc.reader.*;
import sisc.util.Util;
import sisc.util.Defaults;

public class DynamicEnvironment extends Util implements Cloneable {
    public final AppContext ctx;

    public Value in, out;
    
    public Pair sourceAnnotations = EMPTYLIST;

    public Charset characterSet         = Util.getDefaultCharacterSet();
    public boolean caseSensitive        = Defaults.CASE_SENSITIVE;
    public boolean printShared          = Defaults.PRINT_SHARED;
    public boolean vectorLengthPrefixing= Defaults.VECTOR_LENGTH_PREFIXING;
    public boolean emitDebuggingSymbols = Defaults.EMIT_DEBUGGING_SYMBOLS;
    public boolean permissiveParsing    = Defaults.PERMISSIVE_PARSING;
    public boolean hedgedInlining       = Defaults.HEDGED_INLINING;
    public boolean internalDebugging    = Defaults.INTERNAL_DEBUGGING;
    public int     synopsisLength       = Defaults.SYNOPSIS_LENGTH;
    public int     maxStackTraceDepth   = Defaults.MAX_STACK_TRACE_DEPTH;
	public boolean customPrinting       = Defaults.CUSTOM_PRINTING;
	public Pair    customDisplayTypeMap = Util.EMPTYLIST;
	public Pair    customWriteTypeMap   = Util.EMPTYLIST;

    private static String defaultCharacterSet =
        Util.getDefaultCharacterSet().getCharsetName();
    private static String defaultCaseSensitive = 
        new Boolean(Defaults.CASE_SENSITIVE).toString();
    private static String defaultPrintShared =
        new Boolean(Defaults.PRINT_SHARED).toString();
    private static String defaultVectorLengthPrefixing =
        new Boolean(Defaults.VECTOR_LENGTH_PREFIXING).toString();
    private static String defaultEmitDebuggingSymbols =
        new Boolean(Defaults.EMIT_DEBUGGING_SYMBOLS).toString();
    private static String defaultPermissiveParsing =
        new Boolean(Defaults.PERMISSIVE_PARSING).toString();
    private static String defaultHedgedInlining =
        new Boolean(Defaults.HEDGED_INLINING).toString();
    private static String defaultInternalDebugging =
        new Boolean(Defaults.INTERNAL_DEBUGGING).toString();
    private static String defaultSynopsisLength =
        new Integer(Defaults.SYNOPSIS_LENGTH).toString();
    private static String defaultEmitAnnotations =
        new Boolean(Defaults.EMIT_ANNOTATIONS).toString();
    private static String defaultStrictR5RS =
        new Boolean(Defaults.STRICT_R5RS).toString();
    private static String defaultMaxStackTraceDepth =
        new Integer(Defaults.MAX_STACK_TRACE_DEPTH).toString();

    public Value wind = FALSE; //top of wind stack

    //the lexer is stateful
    public Parser parser = new Parser(new Lexer());

    private Thread bindingThread;
    private ClassLoader classLoader;
    private ExtensibleURLClassLoader urlClassLoader;
    private URL[] initialClassPathExtension;

    private static class ExtensibleURLClassLoader extends URLClassLoader {

        public ExtensibleURLClassLoader(URL[] urls, ClassLoader parent) {
            super(urls, parent);
        }

        public void addURL(URL url) {
            super.addURL(url);
        }
    }

    //user-defined thread variables; this map is weak so that we don't
    //hang on to vars that are no longer in use.
    public java.util.Map parameters = new WeakHashMap(1);


    public DynamicEnvironment(AppContext ctx) {
        this(ctx, System.in, System.out);
    }

    public DynamicEnvironment(AppContext ctx, InputStream in, OutputStream out) {
        this.ctx = ctx;
        this.characterSet = Util.charsetFromString(ctx.getProperty("sisc.characterSet", defaultCharacterSet));
        try {
            this.in  = new SchemeCharacterInputPort(new SourceReader(new InputStreamReader(in, this.characterSet.getCharsetName()), liMessage(SISCB, "console")));
            this.out = new SchemeCharacterOutputPort(new BufferedWriter(new OutputStreamWriter(out, this.characterSet.getCharsetName())));
        } catch (UnsupportedEncodingException use) {
            //Hack?
            throw new RuntimeException(use.getMessage());
        }

        this.caseSensitive =
            ctx.getProperty("sisc.caseSensitive", defaultCaseSensitive).equals("true");
        this.printShared =
            ctx.getProperty("sisc.printShared", defaultPrintShared).equals("true");
        this.vectorLengthPrefixing = 
            ctx.getProperty("sisc.vectorLengthPrefixing", defaultVectorLengthPrefixing).equals("true");
        this.emitDebuggingSymbols =
            ctx.getProperty("sisc.emitDebuggingSymbols", defaultEmitDebuggingSymbols).equals("true");
        this.permissiveParsing = 
            ctx.getProperty("sisc.permissiveParsing", defaultPermissiveParsing).equals("true");
        this.hedgedInlining = 
            ctx.getProperty("sisc.hedgedInlining", defaultHedgedInlining).equals("true");
        this.internalDebugging = 
            ctx.getProperty("sisc.internalDebugging", defaultInternalDebugging).equals("true");
        this.synopsisLength = 
            Integer.parseInt(ctx.getProperty("sisc.synopsisLength", defaultSynopsisLength));
        this.parser.annotate =
            ctx.getProperty("sisc.emitAnnotations", defaultEmitAnnotations).equals("true");
        this.parser.lexer.strictR5RS =
            ctx.getProperty("sisc.strictR5RS", defaultStrictR5RS).equals("true");
        this.maxStackTraceDepth =
            Integer.parseInt(ctx.getProperty("sisc.maxStackTraceDepth", defaultMaxStackTraceDepth));

        initialClassPathExtension = new URL[]{};
    }

    public Value getCurrentInPort() {
        return in;
    }

    public Reader getCurrentInReader() {
        return ((SchemeCharacterInputPort) in).getReader();
    }
    
    public Value getCurrentOutPort() {
        return out;
    }
    
    public Writer getCurrentOutWriter() {
        return ((SchemeCharacterOutputPort) out).getWriter();
    }

    public Object clone() throws CloneNotSupportedException {
        DynamicEnvironment res = (DynamicEnvironment)super.clone();
        res.parser = new Parser(new Lexer());
        res.parser.annotate = parser.annotate;
        res.parser.lexer.strictR5RS = parser.lexer.strictR5RS;
        res.initialClassPathExtension = getClassPath();
        res.bindingThread = null;
        res.classLoader = null;
        res.urlClassLoader = null;
        WeakHashMap newParams=new WeakHashMap();
        newParams.putAll(res.parameters);
        res.parameters = newParams;
        return res;
    }

    public DynamicEnvironment copy() {
        try {
            return (DynamicEnvironment)clone();
        } catch (CloneNotSupportedException e) {
            return this;
        }
    }

    /**
     * Binds this DynamicEnvironment to the current thread.
     *
     * DynamicEnvironments must not be used across different
     * threads. This method enforces this. It also initialises the
     * DynamicEnvironments's class loader, which, if the security
     * permissions allow it, will have the current thread's context
     * class loader as a parent.
     */
    public void bind() {
        Thread currentThread = Thread.currentThread();
        if (bindingThread == null) {
            bindingThread = currentThread;
            classLoader = currentClassLoader();
            try {
                urlClassLoader = new ExtensibleURLClassLoader(initialClassPathExtension, classLoader);
            } catch (AccessControlException e) {}
        } else if (bindingThread != currentThread) {
            throw new RuntimeException(liMessage(SISCB, "dynenvrebind"));
        }
    }
        

    public ClassLoader getClassLoader() {
        return (urlClassLoader == null) ? classLoader : urlClassLoader;
    }

    public URL[] getClassPath() {
        return (urlClassLoader == null ?
                new URL[] {} :
                urlClassLoader.getURLs());
    }

    public void extendClassPath(URL url) {
        if (urlClassLoader == null) return;
        urlClassLoader.addURL(url);
    }

    public Value getInputPort() {
        return in;
    }

    public void setInputPort(Value v) {
        in = v;
    }

    public Value getOutputPort() {
        return out;
    }

    public void setOutputPort(Value v) {
        out = v;
    }

    public Value getSourceAnnotations() {
        return sourceAnnotations;
    }

    public void setSourceAnnotations(Value v) {
        sourceAnnotations = (Pair) v;
    }

    public Charset getCharacterSet() {
        return characterSet;
    }

    public void setCharacterSet(Value v) {
        characterSet=Util.charsetFromString(SchemeString.asString(v));
    }
    
    public Value getCaseSensitive() {
        return SchemeBoolean.get(caseSensitive);
    }

    public void setCaseSensitive(Value v) {
        caseSensitive = SchemeBoolean.toBoolean(v);
    }

    public Value getPrintShared() {
        return SchemeBoolean.get(printShared);
    }

    public void setPrintShared(Value v) {
        printShared = SchemeBoolean.toBoolean(v);
    }

    public Value getVectorLengthPrefixing() {
        return SchemeBoolean.get(vectorLengthPrefixing);
    }

    public void setVectorLengthPrefixing(Value v) {
        vectorLengthPrefixing = SchemeBoolean.toBoolean(v);
    }

    public Value getEmitDebuggingSymbols() {
        return SchemeBoolean.get(emitDebuggingSymbols);
    }

    public void setEmitDebuggingSymbols(Value v) {
        emitDebuggingSymbols = SchemeBoolean.toBoolean(v);
    }

    public Value getPermissiveParsing() {
        return SchemeBoolean.get(permissiveParsing);
    }

    public void setPermissiveParsing(Value v) {
        permissiveParsing=SchemeBoolean.toBoolean(v);
    }

    public Value getHedgedInlining() {
        return SchemeBoolean.get(hedgedInlining);
    }
    
    public void setHedgedInlining(Value v) {
        hedgedInlining=SchemeBoolean.toBoolean(v);
    }

    public Value getInternalDebugging() {
        return SchemeBoolean.get(internalDebugging);
    }

    public void setInternalDebugging(Value v) {
        internalDebugging=SchemeBoolean.toBoolean(v);
    }

    public Value getSynopsisLength() {
        return Quantity.valueOf(synopsisLength);
    }

    public void setSynopsisLength(Value v) {
        synopsisLength = ((Quantity) v).intValue();
    }

    public Value getEmitAnnotations() {
        return SchemeBoolean.get(parser.annotate);
    }

    public void setEmitAnnotations(Value v) {
        parser.annotate = SchemeBoolean.toBoolean(v);
    }

    public Value getStrictR5RSCompliance() {
        return SchemeBoolean.get(parser.lexer.strictR5RS);
    }

    public void setStrictR5RSCompliance(Value v) {
        parser.lexer.strictR5RS = SchemeBoolean.toBoolean(v);
    }

    public int getMaxStackTraceDepthAsInt() {
    	return maxStackTraceDepth;
    }
    
    public Value getMaxStackTraceDepth() {
    	return Quantity.valueOf(maxStackTraceDepth);
    }
    
    public void setMaxStackTraceDepth(Value v) {
    	maxStackTraceDepth = ((Quantity) v).indexValue();
    }
    
    public void setCustomPrinting(Value v) {
    	customPrinting=SchemeBoolean.toBoolean(v);
    }
    
    public Value getCustomPrinting() {
    	return SchemeBoolean.get(customPrinting);
    }
    
    public Value getCustomDisplayTypeMap() {
    	return customDisplayTypeMap;
    }
    
    public void setCustomDisplayTypeMap(Value v) {
    	customDisplayTypeMap = (Pair) v;    	
    }

    public Value getCustomWriteTypeMap() {
    	return customWriteTypeMap;
    }
    
    public void setCustomWriteTypeMap(Value v) {
    	customWriteTypeMap = (Pair) v;    	
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
