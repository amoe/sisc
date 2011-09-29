package sisc.interpreter;

import java.io.*;

import sisc.compiler.Compiler;
import sisc.data.*;
import sisc.env.*;
import sisc.ser.Deserializer;
import sisc.ser.Serializer;
import sisc.util.Util;

/**
 * The SISC engine.
 * <p>
 * Interpreter is the SISC engine.  It contains the engine registers,
 * and the main loop responsible for repeatedly executing the
 * <tt>nxp</tt> register and maintaining the stack. Interpreter also
 * localizes all thread-specific information. Interpreters must only
 * execute in the thread which created them. Furthermore, nested calls
 * from Java into Scheme must be carried out in fresh interpreter
 * instances; thus at any point in time a thread contains a stack of
 * interpreters, the top of which is the interpreter currently in use.
 * </p>
 * <p>
 * Additionally, it is the interface from Java code for evaluating
 * Scheme code or calling Scheme procedures.
 * </p>
 * @see Context
 */
public class Interpreter extends Util {

    private final static Expression EVAL_APPEVAL
        = annotatedAppEval("eval");
    private final static Expression CONTINUATION_APPEVAL
        = annotatedAppEval("continuation");

    private static Expression annotatedAppEval(String fn) {
        return annotatedAppEval(Interpreter.class, fn);
    }

    public static class ThrowSchemeException extends Expression {
        
        public void eval(Interpreter r) 
            throws ContinuationException, SchemeRuntimeException {
            r.nxp=null;
            Values v=(Values)r.acc;
            throw new SchemeRuntimeException((Pair) v.values[0], 
                                             (Procedure) v.values[1], 
                                             v.values.length>2 ? 
                                             ((Procedure) v.values[2]) :
                                             //If we are at the top of the
                                             //stack, use a default fk
                                             (r.fk == null ? top_fk : r.fk));
        }

        public Value express() {
            return list(Symbol.get("TSException"));
        }
        
        public void serialize(Serializer s) throws IOException {}
        public void deserialize(Deserializer s) throws IOException {}
    }

    //the compiler is stateless; if that ever changes it would need to
    //be moved to the dynenv
    public static Compiler compiler = new Compiler();

    public ThreadContext tctx;
    public DynamicEnvironment dynenv;
    
    //FLAGS
    private boolean saveVLR; //prevent recycling of VLR after procedure
                             //invocation 

    //Interpreter specific temporaries
    public Value[][]               IAI=new Value[][] {new Value[1],
                                                      new Value[2],
                                                      new Value[3]};

    //ACCOUNTING REGISTERS
    private boolean              vlk;      //vlk, when true, indicates the
                                           //frame was captured.

    //ACTIVITY REGISTERS
    public Value                 acc;    //Accumulator
    public Expression            nxp;    //Next Expression
    public Value[]               vlr,    //Value Rib
                                 lcl,    //Local Variables
                                 env;    //Lexical Variables
    private CallFrame            stk;    //Continuation (Stack)
    public CallFrame             fk;     //Failure Continuation
    public SymbolicEnvironment   tpl;    //Top-level environment

    private StackTracer          tracer; //for stack tracking

    //Scheme->Java exception conversion FK
    static CallFrame top_fk = new CallFrame(new ThrowSchemeException(),
                                            null, false, null, null, null, null, null, null);
    static {
        top_fk.vlk = true;
        // This creates a loop in the stack, which will be a problem for
        // any code checking for null as the bottom of the stack.  However,
        // the only code in SISC which does this is CallFrame.capture(), which
        // will also break when vlk=true.
        top_fk.fk=top_fk;
    }

    public Interpreter(ThreadContext tctx, DynamicEnvironment dynenv) {
        fk=top_fk;
        this.tctx = tctx;
        this.dynenv = dynenv;
        tpl=getCtx().toplevel_env;
    }

    public AppContext getCtx() {
        return dynenv.ctx;
    }

    public Symbol getSymbol(String v) {
        return Symbol.get(v, dynenv.caseSensitive);
    }

    public Expression compile(Value v) throws ContinuationException {
        return compile(v, getCtx().toplevel_env);
    }

    public Expression compile(Value v, SymbolicEnvironment env)
        throws ContinuationException {
        return compiler.compile(this, v, env);
    }

    public Value interpret(Expression e) throws SchemeException {
        SymbolicEnvironment tpl=getCtx().toplevel_env;
        
        stk=createFrame(null, null, false, null, null, tpl, top_fk, null, null);
        tracer = makeStackTracer();
        nxp=e;
        this.tpl=tpl;
        interpret();
        return acc;
    }
    
    protected void interpret() throws SchemeException {
        try {
            do {
                try {
                    do {
                        while (nxp==null) 
                            pop(stk);
                        nxp.eval(this);
                    } while (true);
                } catch (ContinuationException ce) {
                    pop(ce.k);
                }
            } while (true);
        } catch (NullPointerException done) {
            if (nxp!=null) {
                try {
                    error(this, null, done.getMessage(), done);
                } catch (ContinuationException ce) {
                    pop(ce.k);
                    interpret();
                }
            }
        } catch (SchemeRuntimeException rte) {
            throw rte.promote();
        }
    }

    public final void next(Expression nextExpr) throws ContinuationException {
        nxp=nextExpr;
        nextExpr.eval(this);
    }

    public final void newVLR(int size) {
        newVLR(createValues(size));
    }
    
    public final void newVLR(Value[] vlr) {
        if (vlk) {
            tracer = copyStackTracer();
            vlk=false;
        }
        this.vlr=vlr;
    }
    
    public final void pop(CallFrame c) {
        nxp=c.nxp;
        vlr=c.vlr;
        lcl=c.lcl;
        env=c.env;
        tpl=c.tpl;
        fk=c.fk;
        stk=c.parent;
        vlk=c.vlk;
        tracer=c.tracer;
        returnFrame(c);
    }

    public final StackTracer makeStackTracer() {
    	int depth=dynenv.getMaxStackTraceDepthAsInt();
        return (depth == 0 ? null : new StackTracer(depth));
    }

    private final StackTracer copyStackTracer() {
        return (tracer == null ? null : tracer.copy());
    }

    private final void makeSafe() {
        /*
          The frame which contains the current vlr has been captured
          by a continuation. As a result, several, possibly
          concurrent, evaluations may reach it. In order to prevent
          these evaluations from stepping on eachother's toes
          (i.e. modify the same vlr), we copy the vlr before writing
          to it. The copy does not need to be marked as captured since
          it is private to the current computation.
        */
        Value[] newvlr = createValues(vlr.length);
        System.arraycopy(vlr, 0, newvlr, 0, vlr.length);
        vlr = newvlr;
        vlk = false;
        tracer = copyStackTracer();
    }

    public final void setVLR(int pos, Value v) {
        if (vlk) makeSafe();
        vlr[pos]=v;
    }

    private final CallFrame createEmptyFrame(Expression e,
                                             CallFrame p,
                                             StackTracer t) {
        return createFrame(e, null, false, null, null, null, top_fk, p, t);
    }

    private final CallFrame createNearlyEmptyFrame(Expression e,
                                                   CallFrame p,
                                                   StackTracer t) {
        return createFrame(e, null, false, null, null, tpl, fk, p, t);
    }

    public final void pushExpr(Expression e) {
        stk = createNearlyEmptyFrame(e, stk, tracer);
        tracer = makeStackTracer();
    }

    public final void setFailureContinuation(Expression e) {
        fk = createNearlyEmptyFrame(e, stk, copyStackTracer());
    }

    private final Procedure createContinuation(CallFrame p) {
        //In order to produce accurate stack traces for ks we insert a
        //dummy frame with a copy of the current frame's stack trace.
        //The CONTINUATION_APPEVAL nxp of the dummy frame is only
        //there in order to avoid a harmless, but ugly, null nxp.
        //It is never evaluated.
        if (tracer == null) return p;
        else return new ApplyParentFrame(createEmptyFrame(CONTINUATION_APPEVAL, p, tracer.copy()));
    }

    public final Procedure captureContinuation() {
        return createContinuation(stk.capture(this));
    }

    public final Procedure captureEscapingContinuation() {
    	//Even though we're not capturing for long term preservation, we must protect this individual
    	//call frame from being recycled, mostly for error handling.
    	stk.vlk=true;
        return createContinuation(stk);
    }

    public void trace(Expression e) {
        if (tracer != null) {
            if (vlk) {
                if (vlr == null) vlr = ZV; //rare, but can happen
                makeSafe();
            }
            tracer.add(e);
        }
    }

    public void error(Pair error)  throws ContinuationException {
        Procedure k = new ApplyParentFrame(createEmptyFrame(nxp, stk.capture(this), copyStackTracer()));
        acc = new Values(new Value[] { error, k });
        throw new ContinuationException(fk);
    }

    /**
     * Parses and evaluates s-expression(s) from an input port
     * 
     * @param port input port
     * @return The value of the last evaluated s-expression
     * @exception IOException Raised if the port does not
     *     contain a parseable s-expression
     * @exception SchemeException Raised if the evaluation of  
     *      an expression results in an error
     */
    public Value evalInput(PushbackReader port) throws IOException, SchemeException {
        Value rv=VOID;
        do {
            try {
                rv=eval(dynenv.parser.nextExpression(port));
            } catch (EOFException e) {
                return rv;
            }
        } while (true);
    }

    /**
     * Parses and evaluates s-expression(s)
     * 
     * @param expr s-expressions(s)
     * @return The value of the last evaluated s-expression
     * @exception IOException Raised if the given string does not  
     *     contain a parseable s-expression
     * @exception SchemeException Raised if the evaluation of  
     *      an expression results in an error
     */
    public Value eval(String expr) throws IOException, SchemeException {
        return evalInput(new PushbackReader(new BufferedReader(new StringReader(expr))));
    }   

    /**
     * Evaluates a Scheme value as code.  This is equivalent to
     * <tt>(eval <i>v</i>)</tt> in Scheme.
     * 
     * @param v A Scheme Value
     * @return The resulting value
     * @exception SchemeException Raised if the evaluation of the  
     *     expression  results in an error
     */
    public Value eval(Value v) throws SchemeException {
        return eval(v, getCtx().toplevel_env);
    }

    /**
     * Evaluates a Scheme value as code.  This is equivalent to
     * <tt>(eval <i>v</i> <i>e</i>)</tt> in Scheme.
     * 
     * @param v A Scheme Value
     * @param env The environment in which to evaluate the value
     * @return The resulting value
     * @exception SchemeException Raised if the evaluation of the  
     *     expression  results in an error
     */
    public Value eval(Value v, SymbolicEnvironment env) throws SchemeException {
        return eval((Procedure)lookup(EVAL, REPORT), new Value[] {v, env.asValue()});
    }

    /**
     * Applies the given procedure to the given values
     * 
     * @param p A procedure
     * @param args Arguments to call the procedure with
     * @return The result returned by the procedure
     * @exception SchemeException Raised if applying the
     *     procedure results in an error
     */
    public Value eval(Procedure p, Value[] args) throws SchemeException {
        acc = p;
        vlr = args;
        return interpret(EVAL_APPEVAL);
    }

    /**
     * Loads zero or more Scheme source files or compiled libraries.
     *
     * @param files An array of Strings naming files to load.
     * @return true on success, false if any source file produced
     * an error.
     */
    public boolean loadSourceFiles(String[] files) {

        boolean returnStatus = true;
        Procedure load =
            (Procedure)lookup(Symbol.get("load"), Util.TOPLEVEL);

        for (int i=0; i<files.length; i++) {
            try {
                eval(load, new Value[] {new SchemeString(files[i])});
            } catch (SchemeException se) {
                Value vm=se.m;
                try {
                    eval((Procedure)lookup(Symbol.get("print-error"), Util.TOPLEVEL),
                         new Value[] {vm, se.e});
                } catch (SchemeException se2) {
                    if (vm instanceof Pair) {
                        System.err.println(Util.simpleErrorToString((Pair)vm));
                    } else {
                        System.err.println(Util.liMessage(Util.SISCB, "errorduringload")+vm);
                    }
                }
                returnStatus = false;
            }
        }

        return returnStatus;
    }

    public SymbolicEnvironment lookupContextEnv(Symbol s) {
        return getCtx().lookupContextEnv(s);
    }

    public void defineContextEnv(Symbol s, SymbolicEnvironment env) {
        getCtx().defineContextEnv(s, env);
    }

    public SymbolicEnvironment getContextEnv(Symbol s) {
        SymbolicEnvironment contenv=null;
        try {
            contenv = lookupContextEnv(s);
        } catch (ArrayIndexOutOfBoundsException e) {
            contenv=new MemorySymEnv();
            defineContextEnv(s, contenv);
        }
        return contenv;
    }

    /**
     * Defines a new binding in a named environment.
     * 
     * @param s The name of the new binding
     * @param v The value of the new binding
     * @param context The name of the environment in which to  
     *      create the binding
     */
    public void define(Symbol s, Value v, Symbol context) {
        getContextEnv(context).define(s, v);
    }


    /**
     * Retrieves the value of a binding in a named environment
     * 
     * @param s The name of the binding
     * @param context The name of the environment from which   
     *      the  binding will be retrieved
     * @return A value or expression
     */
    public Expression lookup(Symbol s, Symbol context) {
        try {
            return lookupContextEnv(context).lookup(s);
        } catch (ArrayIndexOutOfBoundsException e) {
            return null;
        }
    }

    /**
     * Removes a binding in a named environment
     * 
     * @param s The name of the binding
     * @param context The name of the environment from which   
     *      the  binding will be retrieved
     */
    public void undefine(Symbol s, Symbol context) {
        try {
            lookupContextEnv(context).undefine(s);
        } catch (ArrayIndexOutOfBoundsException e) {}
    }

    //POOLING
    //STATIC --------------------

    protected static final int FRAMEPOOLMAX=128;
    protected CallFrame frameFreeList;
    protected int frameFreeListSize;

    private final CallFrame createFrame(Expression n,
                                        Value[] v,
                                        boolean vk,
                                        Value[] l,
                                        Value[] e,
                                        SymbolicEnvironment t,
                                        CallFrame f,
                                        CallFrame p,
                                        StackTracer tr) {
        CallFrame rv;
        if (frameFreeList == null) {
            rv=new CallFrame();
        } else {
            rv=frameFreeList;
            frameFreeList=frameFreeList.parent;
            frameFreeListSize--;
        }
        rv.init(n,v,vk,l,e,t,f,p,tr);
        return rv;
    }

    public final void push(Expression n) {
        stk = createFrame(n,vlr,vlk,lcl,env,tpl,fk,stk,tracer);
        tracer = makeStackTracer();
    }
    
    public final void returnFrame(CallFrame f) {
        if (f.vlk || frameFreeListSize >= FRAMEPOOLMAX) return;

        //Clear some fields to avoid hanging onto otherwise
        //garbage collectable data for too long
        f.clear();

        f.parent=frameFreeList;
        frameFreeList = f;
        frameFreeListSize++;
    }


    protected Value dv1[], dv2[], dv3[], dv4[];
    
    public final Value[] createValues(int size) {
        Value[] rv;
        switch(size) {
        case 0: return ZV;
        case 1: 
            if (dv1 != null) {
                rv=dv1;
                dv1=null;
                return rv;
            } 
            break;
        case 2: 
            if (dv2 != null) {
                rv=dv2;
                dv2=null;
                return rv;
            } 
            break;
        case 3: 
            if (dv3 != null) {
                rv=dv3;
                dv3=null;
                return rv;
            } 
            break;
        case 4: 
            if (dv4 != null) {
                rv=dv4;
                dv4=null;
                return rv;
            } 
            break;
        }
        return new Value[size];

     }

    public final void returnVLR() {
        if (saveVLR) {
            saveVLR = false;
        } else {
            if (!vlk) 
                returnValues(vlr);
            vlr=null;
        }
    }

    public final void setupTailCall(Expression e, Value vlr0) {
        saveVLR = true;
        nxp = e;
        if (vlk) {
            newVLR(1);
        } else {
            if (vlr.length != 1) {
                returnValues(vlr);
                newVLR(1);
            }
        }
        vlr[0] = vlr0;
    }

    public final void setupTailCall(Expression e, Value[] newvlr) {
        saveVLR = true;
        nxp = e;
        if (!vlk) {
            returnValues(vlr);
        }
        vlr = newvlr;
    }

    public final void returnValues(Value[] v) {
        switch(v.length) {
        case 4: 
            v[3]=v[2]=v[1]=v[0]=null; 
            dv4=v;
            break;
        case 3: 
            v[2]=v[1]=v[0]=null; 
            dv3=v;
            break;
        case 2: 
            v[1]=v[0]=null; 
            dv2=v;
            break;
        case 1: 
            v[0]=null; 
            dv1=v;
            break;
        }
    }

    /**
     * Returns a Value[] prepared as a value rib
     * for a procedure with a fixed argument count.
     * This may or may not clone the VLR depending on
     * whether it is safe to not do so.
     */
    public Value[] vlrToArgs() {
        Value[] vals; 
        if (vlk) {
            vals=createValues(vlr.length);
            System.arraycopy(vlr, 0, vals, 0, vals.length);
        } else {
            vals=vlr;
        }
        return vals;
    }

    /**
     * Returns a Value[] prepared as a value rib for a 
     * for procedure expecting rest args in the last
     * rib position.  This may or may not clone the VLR
     * depending on whether it is safe to not do so.
     * 
     * @param fcount The number of arguments to prepare
     * including the rest variable
     */
    public Value[] vlrToRestArgs(int fcount) {
        Value[] vals;
        int sm1=fcount - 1;
        int vl=vlr.length; 
        
        if (vl < fcount || vlk) {
            /**
             * We must copy the vlr if its locked, 
             * otherwise we may side-effect a captured vlr by 
             * creating the rest argument.
             * 
             * @see Closure.matchArgs
             */
            vals=createValues(fcount);
            System.arraycopy(vlr, 0, vals, 0, sm1);

            vals[sm1]=valArrayToList(vlr, sm1, vl-sm1);
            returnVLR(); //NB: this checks vlk first
        } else {
            vals=vlr;
            vals[sm1]=valArrayToList(vlr, sm1, vl-sm1);
        }
        return vals;
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
