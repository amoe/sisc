package sisc.compiler;

import java.io.InputStreamReader;
import java.util.*;
import sisc.data.*;
import sisc.exprs.*;
import sisc.exprs.fp.*;
import sisc.env.*;
import sisc.nativefun.FixableProcedure;
import sisc.interpreter.Context;
import sisc.interpreter.ContinuationException;
import sisc.interpreter.Interpreter;
import sisc.reader.*;
import sisc.util.FreeReference;

/**
 * Compiler - Compiles regularized Scheme s-expressions into an
 * AST of SISC Expression objects.
 *
 * From v1.9 on, the Compiler expects analyzed s-expressions which 
 * have been annotated with various information about the code
 * including free variables, lexically referenced variables, etc.  
 * The default compile() entrypoint will call the Analyzer implicitly
 * unless instructed not to, so one may still pass fully-expanded but
 * unanalyzed Scheme code.
 *
 * The format for the annotations may change in the future, but is currently:
 * 
 * (program (referenced-var-list ...) (set-var-list ...) (free-var-list ...) 
 *          
 *  <s-expression>)
 * 
 * TODO:  Make output a valid Scheme program by moving #t into the body
 * (#%lambda #t <formals> (lexically-referenced-vars ...) <body>)
 * (#%letrec #t <bindings (lexically-referenced-vars ...) <body>)
 */
public class Compiler extends CompilerConstants {
        
    public Compiler() {}

    public static void addSpecialForms(SymbolicEnvironment menv) {
        for (Iterator i=SYNTACTIC_TOKENS.keySet().iterator(); i.hasNext();) {
            Object ns=i.next();
            if (ns instanceof String) {
                String name=(String)ns;
                extendenv(menv,name,(Syntax)SYNTACTIC_TOKENS.get(name));
            }
        }
    }

    protected Expression compile(Interpreter r, Expression v, Pair sets,
                                 ReferenceFactory rf, 
                                 int context, SymbolicEnvironment env, 
                                 Pair an)
        throws ContinuationException {
        if (v==EMPTYLIST) {
            //we evaluate () to the empty list, which is an "ignorable
            //error", according to R5RS. Note that presently we never
            //actually end up in this code since the macro expander
            //expands () to '().
            return EMPTYLIST;
        } else if (v instanceof Pair) {
            Pair expr=(Pair)v;
            return compileApp(r,expr,sets,rf,context,env,an);
        } else if (v instanceof Symbol) {
            Symbol sym=(Symbol)v;

            Expression ref=rf.createReference(sym, sets, env);
            if (an!=null) setAnnotations(ref, an);
            return ref;
        }
        else return v;
    }

    public Expression compile(Interpreter r, Expression v,
                              SymbolicEnvironment env) 
        throws ContinuationException {
        Expression e=compile(r, v, EMPTYLIST, new ReferenceFactory(),
                             REALTAIL, env, null);
        return e;
    }

    public static final int getExpType(SymbolicEnvironment env, Value s) {
        if (s instanceof Syntax) {
            return ((Syntax)s).synid;
        } else if (s instanceof Symbol){
            Object h = env.lookup((Symbol)s);
            if (h != null && (h instanceof Syntax))
                return ((Syntax) h).synid;
        }
        return APPLICATION;
    }

    static void addAnnotations(Expression e, Map m) {
        if (m!=null)
            if (e.annotations == null)
                e.annotations = m;
            else 
                e.annotations.putAll(m);
    }

    static void setAnnotations(Expression e, Pair p) {
        while (p!=EMPTYLIST) {
            Pair kv=(Pair)p.car();
            e.setAnnotation((Symbol) kv.car(), kv.cdr());
            p=(Pair)p.cdr();
        }
    }

    static void propagateNameAnnotation(Expression from, Expression to) {
        if (from instanceof FreeReferenceExp) {
            to.setAnnotation(PROCNAME, 
                             ((FreeReferenceExp)from).getSym());
        }
    }

    static boolean isImmediate(Expression e) {
        return (e instanceof Immediate) ||
            ((e instanceof AnnotatedExpr) &&
             (((AnnotatedExpr)e).expr instanceof Immediate));
    }

    Expression makeEvalExp(Expression pre, Expression post) {
        EvalExp ee=new EvalExp(pre, post, isImmediate(pre));
        ee.setHosts();
        return ee;
    }

    protected int[][] resolveCopies(ReferenceFactory rf, 
                                    Symbol[] lexicals) {
        List locs=new ArrayList(5), lexs=new ArrayList(5);
        for (int i=lexicals.length-1; i>=0; i--) {
            Ref t=rf.fetchRefType(lexicals[i]);
            if (t!=null) {
                if (t.lcl)
                    locs.add(new Integer(t.idx));
                else lexs.add(new Integer(t.idx));
            }
        }
        int rv[][]=new int[2][];
        
        rv[0]=new int[locs.size()];
        rv[1]=new int[lexs.size()];
        for (int i=0; i<locs.size(); i++) {
            rv[0][i]=((Integer)locs.get(i)).intValue();
        }
                                
        for (int i=0; i<lexs.size(); i++) {
            rv[1][i]=((Integer)lexs.get(i)).intValue();
        }
        return rv;
    }

    public int[] findBoxes(Symbol[] formals, Pair sets) {
        ArrayList v=new ArrayList(5);
        for (int i=0; i<formals.length; i++) {
            if (assq(formals[i], sets)!=FALSE)
                v.add(new Integer(i));
        }
        int[] boxes=new int[v.size()];
        for (int i=0; i<boxes.length; i++)
            boxes[i]=((Integer)v.get(i)).intValue();
        return boxes;
    }

    public Expression compileApp(Interpreter r,
                                 Pair expr, Pair sets, ReferenceFactory rf,
                                 int context, SymbolicEnvironment env,
                                 Pair an)
    throws ContinuationException {

        Expression tmp, rv;
        
        Value oper=expr.car();
        int eType=getExpType(env, oper);
        expr=(Pair)expr.cdr();
        
        switch (eType) {
        case QUOTE:
            rv=expr.car();
            break;
        case PROGRAM:
            //References
            expr=(Pair)expr.cdr();
            //Sets
            sets=append((Pair)expr.car(), sets);
            expr=(Pair)expr.cdr();
            //Frees
            expr=(Pair)expr.cdr();
            rv=compile(r, expr.car(), sets, rf, context, env, null);
            break;
        case LAMBDA:
         {
             boolean infArity=false;
             // Skip #t
             expr=(Pair)expr.cdr();
 
             Symbol[] formals=null;
             Value ftmp=expr.car();
             if (ftmp instanceof Pair && ftmp != EMPTYLIST) {
                 formals=argsToSymbols((Pair)ftmp);
                 do {
                     ftmp=((Pair)ftmp).cdr(); 
                 } while (ftmp != EMPTYLIST && ftmp instanceof Pair);
                 infArity=ftmp instanceof Symbol;
             } else if (expr.car() instanceof Symbol) {
                 formals=new Symbol[] {(Symbol)expr.car()};
                 infArity=true;
             } else {
                 formals=new Symbol[0];
             }
             
             expr=(Pair)expr.cdr();
 
             Symbol[] lexicalSyms=argsToSymbols((Pair)expr.car());
             expr=(Pair)expr.cdr();
                          ReferenceFactory nf=new ReferenceFactory(formals, lexicalSyms);
 
             tmp=compile(r, expr.car(), sets, nf, REALTAIL, env, null);
             int[][] copies=resolveCopies(rf, lexicalSyms);
             int[] boxes=findBoxes(formals, sets);
             rv=new LambdaExp(formals.length, 
                              tmp, infArity, copies[0], copies[1], 
                              (boxes.length==0 ? null :boxes));
         }
            break;
        case LETREC:
         {
             // Skip the #t marker
             expr=(Pair)expr.cdr();
 
             Pair tmpp=(Pair)expr.car();
 
             Vector formv=new Vector();
             Vector expv=new Vector();
             
             while (tmpp != EMPTYLIST) {
                 Pair bp=(Pair)tmpp.car();
 
                 formv.add(bp.car());
                 expv.add(((Pair)bp.cdr()).car());
                 tmpp=(Pair)tmpp.cdr();
             }
             
             Symbol[] formals=new Symbol[formv.size()];
             Expression[] rhses=new Expression[expv.size()];
             formv.copyInto(formals);
             expv.copyInto(rhses);
 
             expr=(Pair)expr.cdr();
             Symbol[] lexicalSyms=argsToSymbols((Pair)expr.car());
             expr=(Pair)expr.cdr();
             
             rv=compileLetrec(r, formals, lexicalSyms, rhses, expr.car(), 
                              sets, rf, env, context);
         }
         break;
        case _IF:
            tmp=compile(r, expr.car(), sets, rf, 0, env, null);
            expr=(Pair)expr.cdr();
            
            Expression conseq=compile(r, expr.car(), sets, rf, 0,
                                      env, null);
            expr=(Pair)expr.cdr();
            Expression altern=compile(r, expr.car(), sets, rf, 0,
                                      env, null);
            rv=new IfEval(conseq, altern);
            ((OptimisticHost)rv).setHosts();
            rv.annotations = tmp.annotations;
            rv = makeEvalExp(tmp, rv);
            break;
        case BEGIN:
            rv=compileBegin(r, pairToExpressions(expr), context, sets, rf, env);
            break;
        case SET:
            Symbol sym=(Symbol)expr.car();
            tmp=compile(r, sym, sets, rf, 0, env, null);
            expr=(Pair)expr.cdr();
            Expression rhs=compile(r, expr.car(), sets, rf, 0, env, null);
            if (tmp instanceof FreeReferenceExp) 
                rv=new FreeSetEval(sym, env);
            else
                rv=new SetboxEval(((UnboxExp)tmp).ref);
            rv.annotations = rhs.annotations;
            rv= makeEvalExp(rhs, rv);
            break;
        case DEFINE:
            Symbol lhs=(Symbol)expr.car();
            expr=(Pair)expr.cdr();
            rhs = compile(r, expr.car(), sets, rf, 0, env, null);
            rv = new DefineEval(lhs, env);
            addAnnotations(rv, rhs.annotations);
            rv = makeEvalExp(rhs, rv);
            break;
        case MAKEANNOTATION:
            Value aexpr=expr.car();
            expr=(Pair)expr.cdr();
            Pair annot=null;
            if (expr.car() instanceof Pair)
                annot=(Pair) expr.car();
            else
                annot=list(new Pair(OTHER, expr.car()));
            rv=compile(r, aexpr, sets, rf, context, env, annot);
            an=null;
            break;
        case APPLICATION:
        case UNKNOWN:
            Expression[] exps=pairToExpressions(expr);
            compileExpressions(r, exps, sets, rf, 0, env);
            Expression operout=compile(r,oper,sets, rf,0,env,an);
            rv = application(r, operout, exps, context, an, env);
            break;
        default:
            error(r, "Unsupported syntactic type ["+eType+"].  Should never happen!");
            rv=null;
        }
        if (an!=null)
            setAnnotations(rv, an);
        return rv;
    }

    public static final Expression makeFillRib(Interpreter r,
                                               Expression lastRand,
                                               Expression rand,
                                               int pos,
                                               Expression nxp,
                                               boolean immediate) {
        nxp = new FillRibExp(lastRand, pos, nxp, immediate);
        addAnnotations(nxp, rand.annotations);

        /* If we're emitting debugging symbols, annotate the
         * FillRibExps with the names of the functions in the operator
         * position.
         */
        if (r.dynenv.emitDebuggingSymbols && rand instanceof AppExp) {
            AppExp ae=(AppExp)rand;
            propagateNameAnnotation(ae.exp, nxp);
        }

        return nxp;
    }

   public Expression compileLetrec(Interpreter r,
                                    Symbol[] formals, Symbol[] lexicals,
                                    Expression[] rands,
                                    Expression body, Pair sets, 
                                    ReferenceFactory rf, 
                                    SymbolicEnvironment env, int context) 
        throws ContinuationException {
        ReferenceFactory nrf=new ReferenceFactory(formals, lexicals);
        compileExpressions(r, rands, sets, nrf, 0, env);
        boolean allImmediate=true;

        Expression nxp=new LetrecEval(compile(r, body, sets, nrf,
                                              0, env, null));
        ((OptimisticHost)nxp).setHosts();
        
        /* If we're emitting debugging symbols, annotate the LetrecEval
           with the name of the procedure. 
        */
        if (r.dynenv.emitDebuggingSymbols)
            nxp.setAnnotation(PROCNAME, _LETREC);

        Expression lastRand = VOID;

        for (int i= 0; i<rands.length; i++) {
            if (!isImmediate(rands[i])) {
                nxp = makeFillRib(r, lastRand, rands[i], i, nxp, allImmediate);
                lastRand = rands[i];
                rands[i] = null;
                allImmediate=false;
            }
        }

        int[][] copies=resolveCopies(rf, lexicals);
        LetrecExp res = new LetrecExp(lastRand, rands, nxp,
                                      copies[0], copies[1], allImmediate);
        res.setHosts();
        return res;
    }

    public static final Expression application(Interpreter r, 
                                        Expression rator, Expression rands[], 
                                        int context, Pair annotation,
                                        SymbolicEnvironment env) throws ContinuationException{
        if (rator instanceof Value && 
            !(rator instanceof Procedure) &&
            !(rator instanceof AnnotatedExpr)) {
            System.err.println(warn("nonprocappdetected",((Value)rator).synopsis()));
        }
        Expression nxp = new AppEval();
        
        if (annotation!=null)
            setAnnotations(nxp, annotation);

        /* If we're emitting debugging symbols, annotate the AppEval
           with the name of the procedure. 
        */
        if (r.dynenv.emitDebuggingSymbols) {
            propagateNameAnnotation(rator, nxp);
        }

        Expression lastRand = rator;
        boolean allImmediate=isImmediate(rator);

        addAnnotations(nxp, lastRand.annotations);

        for (int i= 0; i<rands.length; i++) {
            if (!isImmediate(rands[i])) {
                nxp = makeFillRib(r, lastRand, rands[i], i, nxp, allImmediate);
                lastRand = rands[i];
                rands[i] = null;
                allImmediate=false;
            }
        }

        if (allImmediate &&
            rator instanceof FreeReferenceExp &&
            //The realtail check is necessary since an Optimistic
            //Expression in real tail has no parent uExp
            (context & REALTAIL)== 0) {
            FreeReference ref=((FreeReferenceExp)rator).getReference();
            Symbol ratorsym=ref.getName();
            Value ratorval=env.lookup(ratorsym);
            if (ratorval instanceof FixableProcedure) {
                Expression fixedCall=null;

                switch (rands.length) {
                  case 0: 
                      fixedCall = new FixedAppExp_0(ref);
                      break;
                  case 1: 
                      fixedCall = new FixedAppExp_1((Immediate)rands[0],
                                                    ref);
                      break;
                  case 2: 
                      fixedCall = new FixedAppExp_2((Immediate)rands[0],
                                                    (Immediate)rands[1],
                                                    ref);
                      break;
                  case 3: 
                      fixedCall = new FixedAppExp_3((Immediate)rands[0],
                                                    (Immediate)rands[1],
                                                    (Immediate)rands[2],
                                                    ref);
                      break;
                }
                if (fixedCall != null) {
                    if (annotation!=null)
                        setAnnotations(fixedCall, annotation);
                    if (r.dynenv.emitDebuggingSymbols) {
                        propagateNameAnnotation(rator, fixedCall);
                    }
                    addAnnotations(fixedCall, lastRand.annotations);
                    if (fixedCall instanceof OptimisticHost)
                        ((OptimisticHost)fixedCall).setHosts();
                    if (!r.dynenv.hedgedInlining) {
                        ((OptimisticExpression)fixedCall).dropSafe();
                    }
                    return fixedCall;
                }
            }
        }

        AppExp res = new AppExp(lastRand, rands, nxp, allImmediate);
        if (annotation!=null)
            setAnnotations(res, annotation);
        res.setHosts();
        return res;
    }

    void compileExpressions(Interpreter r, Expression exprs[], 
                            Pair sets, ReferenceFactory rf, 
                            int context, SymbolicEnvironment env)
        throws ContinuationException {
        for (int i=exprs.length-1; i>=0; i--) 
            exprs[i]=compile(r, exprs[i], sets, rf,context, env, null);
    }

    Expression compileBegin(Interpreter r, Expression[] v, int context,
                            Pair sets, ReferenceFactory rf,
                            SymbolicEnvironment env)
    throws ContinuationException {        
        Expression last=compile(r, v[v.length - 1], sets, rf,
                                (v.length > 1 ? REALTAIL : 0), env, null);
        Expression be=last;
        for (int i = v.length - 2; i >= 0; --i) {
            Expression e=compile(r, v[i], sets, rf, 0, env, null);
            addAnnotations(be, e.annotations);
            be = makeEvalExp(e, be);
        }
        return be;
    }

    static class Ref {
        int idx;
        boolean lcl;

        public Ref(boolean lcl, int idx) {
            this.lcl=lcl;
            this.idx=idx;
        }
    }
            
    static class ReferenceFactory {
        Symbol[] locals, lexicals;

        public ReferenceFactory() {}

        public ReferenceFactory(Symbol[] lcls, Symbol[] lxcls) {
            locals=lcls;
            lexicals=lxcls;
        }

        public Ref fetchRefType(Symbol s) {
            if (locals!=null) 
                for (int i=locals.length-1; i>=0; i--) {
                    if (locals[i]==s) 
                        return new Ref(true, i);
                }
            if (lexicals != null) 
                for (int i=lexicals.length-1; i>=0; i--) {
                    if (lexicals[i]==s) 
                        return new Ref(false, i);
                }
            return null;
        }
            
        public Expression createReference(Symbol s, Pair sets,
                                          SymbolicEnvironment env) {
            Ref r=fetchRefType(s);

            if (r==null) 
                return new FreeReferenceExp(s,env);

            Immediate ref;
            if (r.lcl) {
                ref = new LocalReferenceExp(r.idx);
            } else {
                ref = new LexicalReferenceExp(r.idx);
            }

            return (assq(s, sets)!=FALSE) ?
                new UnboxExp(ref) :
                (Expression)ref;
        }
    }

    public static void main(String[] args) throws Exception {
        Interpreter r=Context.enter();
        Parser p=new Parser(new Lexer());
        SourceReader in=new SourceReader(new InputStreamReader(System.in), "stdin");
        SymbolicEnvironment env=new MemorySymEnv();
        Compiler.addSpecialForms(env);
        new sisc.modules.Primitives.Index().bindAll(r, env);        
        Compiler c=new Compiler();
        Expression v=c.compile(r, p.nextExpression(in), env);
        System.out.println(v.express());
        System.err.println(r.interpret(v));
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
