package sisc.modules;

import sisc.data.*;
import sisc.data.proc.Closure;
import sisc.exprs.*;
import sisc.interpreter.*;
import sisc.nativefun.*;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;
import java.net.URL;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import sisc.env.ConfigParameter;
import sisc.env.NativeParameter;
import sisc.env.Parameter;
import sisc.env.SchemeParameter;
import sisc.env.SymbolicEnvironment;
import sisc.env.MemorySymEnv;
import sisc.util.*;

public abstract class Primitives extends Util {

    final static String GENSYM_MAGIC_PREFIX = "%%_";
    
    static final char[] b64cs=
        ("0123456789abcdefghijklmnopqrstuvwxyz"+
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ-_").toCharArray();

    private final static Expression CALLEC_APPEVAL
        = annotatedAppEval("call-with-escape-continuation");
    private final static Expression CALLCC_APPEVAL
        = annotatedAppEval("call-with-current-continuation");
    private final static Expression CALLFC_APPEVAL
        = annotatedAppEval("call-with-failure-continuation");
    private final static Expression WITHFC_APPEVAL
        = annotatedAppEval("with-failure-continuation");
    private final static Expression WITHENV_APPEVAL
        = annotatedAppEval("_with-environment");
    private final static Expression CALLWITHVALUES_APPEVAL
        = annotatedAppEval("call-with-values");
    private final static Expression APPLY_APPEVAL
        = annotatedAppEval("apply");

    private static Expression annotatedAppEval(String fn) {
        return annotatedAppEval(Primitives.class, fn);
    }

    protected static String base64encode(long v) {
        StringBuffer b=new StringBuffer();
        while (v!=0) {
            b.append(b64cs[(int)v & 0x3f]);
            v>>>=6;
        }
        return b.toString();
    }
   
    public static SchemeBoolean numQuery(Value v, int mask)
        throws ContinuationException {
        return SchemeBoolean.get(v instanceof Quantity &&
                     (((Quantity)v).is(mask)));
    }

    public static class CircularityDetector implements ExpressionVisitor {

        private Map trailMap;
        private LinkedList trail;
        private ExpressionVisitee element;
        private LinkedList components;

        public CircularityDetector() {
            trailMap = new HashMap(1);
            trail = new LinkedList();
        }

        public boolean isCircular(ExpressionVisitee e) {
            element = e;
            components = null;
            boolean res = isCircular();
            element = null;
            components = null;
            trailMap.clear();
            trail.clear();
            return res;
        }

        private boolean isCircular() {
            // this loop is complicated by an optimisation: checking
            // non-composite elements does not require any memory
            // allocation.
            while(element.visit(this)) {
                if (components == null) {
                    while(true) {
                        if (trail.isEmpty()) return false;
                        element = (ExpressionVisitee)trail.getLast();
                        components = (LinkedList)trailMap.get(element);
                        if (!components.isEmpty()) break;
                        // pop trail element
                        trail.removeLast();
                        trailMap.remove(element);
                    }
                } else {
                    // push trail element
                    trailMap.put(element, components);
                    trail.addLast(element);
                }
                // pop component
                element = (ExpressionVisitee)components.removeFirst();
                components = null;
            }
            return true;
        }

        public boolean visit(ExpressionVisitee e) {
            if (e == null) return true;
            if (element.equals(e) || trailMap.containsKey(e))
                return false;
            if (components == null) components = new LinkedList();
            // push component
            components.addLast(e);
            return true;
        }
    }

    public static class Index extends IndexedLibraryAdapter {
               
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
        }
        
        public Index() {
            define("apply", Complex.class, APPLY);
            define("call-with-current-continuation", Complex.class, CALLCC);
            define("call-with-escape-continuation", Complex.class, CALLEC);
            define("call-with-failure-continuation", Complex.class, CALLFC);
            define("call-with-values", Complex.class, CALLWITHVALUES);
            define("compact-string-rep", Complex.class, COMPACTSTRINGREP);
            define("compile", Complex.class, COMPILE);
            define("current-wind", Complex.class, CURRENTWIND);
            define("gensym", Complex.class, GENSYM);
            define("gensym?", GENSYMQ);
            define("getenv", Complex.class, GETENV);
            define("getprop", Complex.class, GETPROP);
            define("get-sidecar-environment", Complex.class, GETSIDECAR);
            define("get-symbolic-environment", Complex.class, GETENVIRONMENT);
            define("set-symbolic-environment!", Complex.class, SETENVIRONMENT);
            define("interaction-environment", Complex.class, INTERACTIONENVIRONMENT);
            define("load-native-library", Complex.class, LOADNL);
            define("native-library-binding", Complex.class, NLBINDING);
            define("native-library-binding-names", Complex.class, NLBINDINGNAMES);
            define("number->string", Complex.class, NUMBER2STRING);
            define("parent-environment", Complex.class, PARENTENVIRONMENT);
            define("putprop", Complex.class, PUTPROP);
            define("remprop", Complex.class, REMPROP);
            define("scheme-report-environment", Complex.class, REPORTENVIRONMENT);
            define("seal-immutable-pair!", Complex.class, SEALIMMUTABLEPAIR);
            define("seal-immutable-vector!", Complex.class, SEALIMMUTABLEVECTOR);
            define("sisc-initial-environment", Complex.class, SISCINITIAL);
            define("string-fill!", Complex.class, STRINGFILL);
            define("string-set!", Complex.class, STRINGSET);
            define("string->number", Complex.class, STRING2NUMBER);
            define("vector-fill!", Complex.class, VECTORFILL);
            define("vector-set!", Complex.class, VECTORSET);
            define("with-failure-continuation", Complex.class, WITHFC);
            define("_with-environment", Complex.class, WITHENVIRONMENT);
            define("class-path-extension", Complex.class, CLASSPATHEXTENSION);
            define("class-path-extension-append!", Complex.class, CLASSPATHEXTENSIONAPPEND);
            
            define("list", LIST);
            define("*", MUL);
            define("+", ADD);
            define("-", SUB);
            define("/", DIV);
            define("<", LT);
            define("=", NEQ);
            define(">", GRT);
            define("_gcd", GCD);
            define("_lcm", LCM);
            define("string-append", STRINGAPPEND);
            define("acos", ACOS);
            define("ashl", ASHL);
            define("ashr", ASHR);
            define("asin", ASIN);
            define("atan", ATAN);
            define("boolean?", BOOLEANQ);
            define("box", BOX);
            define("box?", BOXQ);
            define("car", CAR);
            define("cdr", CDR);
            define("ceiling", CEILING);
            define("char->integer", CHAR2INTEGER);
            define("char?", CHARACTERQ);
            define("char=?", CHAREQUAL);
            define("circular?", CIRCULARQ);
            define("complex?", COMPLEXQ);
            define("cons", CONS);
            define("cons-immutable", CONSIMMUTABLE);
            define("cos", COS);
            define("denominator", DENOMINATOR);
            define("environment?", ENVIRONMENTQ);
            define("eq?", EQ);
            define("eqv?", EQV);
            define("equal?", EQUAL);
            define("exact->inexact", EXACT2INEXACT);
            define("exact?", EXACTQ);
            define("_expression-type", EXPTYPE);
            define("exp", EXP);
            define("find-last-unique-vector-element", VECTORFINDLASTUNIQUE);
            define("floor", FLOOR);
            define("hash-code", HASHCODE);
            define("imag-part", IMAGPART);
            define("immutable-pair?", IMMUTABLEPAIRQ);
            define("immutable-vector?", IMMUTABLEVECTORQ);
            define("inexact->exact", INEXACT2EXACT);
            define("inexact?", INEXACTQ);
            define("integer->char", INTEGER2CHAR);
            define("_integer?", INTEGERQ);
            define("intern", INTERN);
            define("length", LENGTH);
            define("list->vector", LIST2VECTOR);
            define("log", LOG);
            define("_make-parameter", MAKEPARAM);
            define("_make-native-parameter", MAKENATIVEPARAM);
            define("_make-config-parameter", MAKECONFIGPARAM);
            define("make-child-environment", MAKECHILDENVIRONMENT); 
            define("make-rectangular", MAKERECTANGULAR);
            define("make-string", MAKESTRING);
            define("make-vector", MAKEVECTOR);
            define("make-immutable-vector", MAKEIMMUTABLEVECTOR);
            define("native-library-name", NLNAME);
            define("native-library-version", NLVERSION);
            define("max-float-precision", MAXFLOATPRECISION);
            define("min-float-precision", MINFLOATPRECISION);
            define("null-environment", NULLENVIRONMENT);
            define("null?", NULLQ);
            define("number?", NUMBERQ);
            define("numerator", NUMERATOR);
            define("pair?", PAIRQ);
            define("parameter?", PARAMETERQ);
            define("permit-interrupts", PERMITINTERRUPTS);
            define("procedure?", PROCEDUREQ);
            define("quotient", QUOTIENT);
            define("real-part", REALPART);
            define("remainder", REMAINDER);
            define("round", ROUND);
            define("set-box!", Complex.class, SETBOX);
            define("set-car!", Complex.class, SETCAR);
            define("set-cdr!", Complex.class, SETCDR);
            define("_signal-hook!", Complex.class, SIGHOOK);
            define("_signal-unhook!", Complex.class, SIGUNHOOK);
            define("sin", SIN);
            define("sqrt", SQRT);
            define("sleep", SLEEP);
            define("string->symbol", STRING2SYMBOL);
            define("string->uninterned-symbol", STRING2UNINTERNEDSYMBOL);
            define("string-length", STRINGLENGTH);
            define("string=?", STRINGEQUAL);
            define("string-ref", STRINGREF);
            define("string?", STRINGQ);
            define("symbol->string", SYMBOL2STRING);
            define("symbol?", SYMBOLQ);
            define("syntactic-token?", SYNTOKENQ);
            define("syntactic-token->string", SYNTOKEN2STRING);
            define("system-time", SYSTIME);
            define("tan", TAN);
            define("time-zone-offset", TIMEZONEOFFSET);
            define("truncate", TRUNCATE);
            define("unbox", UNBOX);
            define("vector->list", VECTOR2LIST);
            define("vector-length", VECTORLENGTH);
            define("vector-ref", VECTORREF);
            define("vector?", VECTORQ);
            define("void", _VOID);
            define("void?", VOIDQ);
        }
    }
    
    /**
     * The Simple procedures are purely functional procedures which do not need
     * to access interpreter registers to execute
     */
    public static class Simple extends IndexedFixableProcedure {
        public Simple() {}
        
        Simple(int id) {
            super(id);
        }
        
        public final Value apply() throws ContinuationException {
            switch (id) {
            case ADD: return Quantity.ZERO;
            case MAXFLOATPRECISION: return Quantity.valueOf(maxFloatPrecision);
            case MINFLOATPRECISION: return Quantity.valueOf(minFloatPrecision);
            case MUL: return Quantity.ONE;
            case PERMITINTERRUPTS: return SchemeBoolean.get(permitInterrupts);
            case SYSTIME: return Quantity.valueOf(System.currentTimeMillis());
            case TIMEZONEOFFSET:
                Calendar cal = Calendar.getInstance();
                return Quantity.valueOf((cal.get(Calendar.ZONE_OFFSET) + cal.get(Calendar.DST_OFFSET)) / 1000); 
            case _VOID: return VOID;
            case LIST: return EMPTYLIST;
            case STRINGAPPEND:
                return new SchemeString("");
            default:
                throwArgSizeException();
                return VOID;
            }
        }

        public final Value apply(Value v1) 
            throws ContinuationException {      
            switch (id) {
            case NULLQ: return SchemeBoolean.get(v1==EMPTYLIST);
            case CAR: return truePair(v1).car();
            case CDR: return truePair(v1).cdr();
            case PAIRQ:
                return SchemeBoolean.get(v1 instanceof Pair &&
                             v1!=EMPTYLIST);
            case IMMUTABLEPAIRQ:
                return SchemeBoolean.get((v1 instanceof ImmutablePair) &&
                             ((ImmutablePair)v1).isImmutable());
            case IMMUTABLEVECTORQ:
                return SchemeBoolean.get(v1 instanceof ImmutableVector);
            case ADD: 
            case MUL: return (Quantity) v1;
            case SUB: return ((Quantity) v1).negate();
            case DIV: return Quantity.ONE.div((Quantity) v1);
            case SIN: return ((Quantity) v1).sin();
            case COS: return ((Quantity) v1).cos();
            case TAN: return ((Quantity) v1).tan();
            case ASIN: return ((Quantity) v1).asin();
            case ACOS: return ((Quantity) v1).acos();
            case ATAN: return ((Quantity) v1).atan();
            case LOG: return ((Quantity) v1).log();
            case EXP: return ((Quantity) v1).exp();
            case SQRT: return ((Quantity) v1).sqrt();
            case NUMBERQ: return SchemeBoolean.get(v1 instanceof Quantity);
            case VECTORQ: return SchemeBoolean.get(v1 instanceof SchemeVector);
            case SYMBOLQ: return SchemeBoolean.get(v1 instanceof Symbol);
            case SYNTOKENQ: return SchemeBoolean.get(v1 instanceof sisc.compiler.Syntax);
            case CHARACTERQ: return SchemeBoolean.get(v1 instanceof SchemeCharacter);
            case STRINGQ: return SchemeBoolean.get(v1 instanceof SchemeString);
            case BOOLEANQ: return SchemeBoolean.get(v1 instanceof SchemeBoolean);
            case VOIDQ: return SchemeBoolean.get(v1==VOID);
            case ENVIRONMENTQ: return SchemeBoolean.get(v1 instanceof SymbolicEnvironment);
            case PROCEDUREQ: return SchemeBoolean.get(v1 instanceof Procedure);
            case INTEGERQ: return numQuery(v1,Quantity.INTEGER);
                
            case COMPLEXQ: return numQuery(v1,Quantity.IMAGINARY);
            case EXACTQ: return numQuery(v1,Quantity.EXACT);
            case INEXACTQ: return numQuery(v1,Quantity.INEXACT);
            case PARAMETERQ: return SchemeBoolean.get(v1 instanceof Parameter);
            case GENSYMQ:
                return SchemeBoolean.get(((Symbol) v1).symval.startsWith(GENSYM_MAGIC_PREFIX));
            case SYMBOL2STRING:
                return new ImmutableString(((Symbol) v1).symval);
            case SYNTOKEN2STRING: 
                return new ImmutableString(((sisc.compiler.Syntax)v1).toString());

            case STRING2SYMBOL: return Symbol.intern(SchemeString.asString(v1));
            case CHAR2INTEGER: return Quantity.valueOf(SchemeCharacter.charValue(v1));
            case LIST2VECTOR: return new SchemeVector(Util.pairToValues((Pair) v1));
            case VECTOR2LIST:
                Value[] vals=((SchemeVector) v1).vals;
                return valArrayToList(vals, 0, vals.length);
            case EXACT2INEXACT: return ((Quantity) v1).toInexact();
            case INEXACT2EXACT: return ((Quantity) v1).toExact();
            case FLOOR: return ((Quantity) v1).floor();
            case CEILING: return ((Quantity) v1).ceiling();
            case ROUND: return ((Quantity) v1).round();
            case TRUNCATE: return ((Quantity) v1).truncate();
            case INTEGER2CHAR: return new SchemeCharacter((char)((Quantity) v1).
                                                          indexValue());
            case VECTORFINDLASTUNIQUE: return Quantity.valueOf(((SchemeVector) v1).findEnd());
            case BOX: return new Box(v1);
            case UNBOX: return ((Box) v1).val;
            case BOXQ: return SchemeBoolean.get(v1 instanceof Box);
            case LENGTH:
                return Quantity.valueOf(length((Pair) v1));
            case STRINGLENGTH:
                return Quantity.valueOf(((SchemeString) v1).length());
            case VECTORLENGTH:
                return Quantity.valueOf(((SchemeVector) v1).vals.length);
            case CIRCULARQ:
                return SchemeBoolean.get(new CircularityDetector().isCircular(v1));
            case MAKEPARAM:
                return new SchemeParameter(v1);
            case MAKENATIVEPARAM:
                return new NativeParameter(SchemeString.asString(v1));
            case MAKESTRING:
                return new SchemeString(new char[((Quantity) v1).indexValue()]);
            case MAKEVECTOR:
                return new SchemeVector(((Quantity) v1).indexValue());
            case MAKEIMMUTABLEVECTOR:
                return new ImmutableVector(((Quantity) v1).indexValue(), false);
            case NUMERATOR: return ((Quantity) v1).numerator();
            case DENOMINATOR: return ((Quantity) v1).denominator();
            case REALPART: return ((Quantity) v1).realpart();
            case IMAGPART: return ((Quantity) v1).imagpart();
            case STRING2UNINTERNEDSYMBOL:
                return Symbol.getUnique(SchemeString.asString(v1));
            case MAKECHILDENVIRONMENT:
                SymbolicEnvironment env=(SymbolicEnvironment) v1;
                MemorySymEnv ae = new MemorySymEnv(env);
                sisc.compiler.Compiler.addSpecialForms(ae);
                return ae;
            case NULLENVIRONMENT:
                switch (((Quantity) v1).indexValue()) {
                case 5:
                    ae = new MemorySymEnv();
                    sisc.compiler.Compiler.addSpecialForms(ae);
                    return ae;
                case 0:
                    return new MemorySymEnv();
                default:
                    throwPrimException(liMessage(SISCB, "unsupportedstandardver"));
                    return VOID;
                }
            case NLNAME:
                return Symbol.get(((NativeLibrary) v1).getLibraryName());
            case NLVERSION:
                return Quantity.valueOf(((NativeLibrary) v1).getLibraryVersion());
            case SLEEP:
                try {
                    Thread.sleep(((Quantity) v1).longValue());
                } catch (InterruptedException ie) {}
                return VOID;
            case STRINGAPPEND:
                return ((SchemeString) v1).copy();
            case LIST: return new Pair(v1, EMPTYLIST);
            case HASHCODE:
                return Quantity.valueOf(v1.hashCode());
            default:
                throwArgSizeException();
                return VOID;
            }
        }

        public final Value apply(Value v1, Value v2) 
            throws ContinuationException {      
            switch (id) {
            case EQ: return SchemeBoolean.get(v1 == v2);
            case EQV: return SchemeBoolean.get(v1.eqv(v2));
            case CONS:
                return new Pair(v1, v2);
            case CONSIMMUTABLE:
                return new ImmutablePair(v1, v2, false);
            case EQUAL:
                return SchemeBoolean.get(v1.valueEqual(v2));
            case EXPTYPE:
                return Quantity.valueOf(sisc.compiler.Compiler.getExpType((SymbolicEnvironment) v1, v2));
            case CHAREQUAL:
                return SchemeBoolean.get(SchemeCharacter.charValue(v1) == SchemeCharacter.charValue(v2));
            case ADD: return ((Quantity) v1).add((Quantity) v2);
            case MUL: return ((Quantity) v1).mul((Quantity) v2);
            case SUB: return ((Quantity) v1).sub((Quantity) v2);
            case DIV: return ((Quantity) v1).div((Quantity) v2);
            case NEQ: return SchemeBoolean.get(((Quantity) v1).comp((Quantity) v2,0));
            case REMAINDER:
                return ((Quantity) v1).remainder((Quantity) v2);
            case QUOTIENT:
                return ((Quantity) v1).quotient((Quantity) v2);
            case LCM:
                return ((Quantity) v1).lcm((Quantity) v2);
            case GCD:
                return ((Quantity) v1).gcd((Quantity) v2);
            case ATAN:
                return ((Quantity) v1).atan((Quantity) v2);
            case STRINGREF:
                int index=((Quantity) v2).indexValue();
                try {
                    return new SchemeCharacter(((SchemeString) v1).charAt(index));
                } catch (ArrayIndexOutOfBoundsException e) {
                    throwPrimException(liMessage(SISCB, "indexoob", 
                                                 new Object[] {
                                                     new Integer(index),
                                                     v1.synopsis()}));
                }
            case VECTORREF:
                index=((Quantity) v2).indexValue();
                return ((SchemeVector) v1).vals[index];
            case STRINGEQUAL:
                return SchemeBoolean.get(((SchemeString) v1).valueEqual((SchemeString) v2));
            case MAKEVECTOR:
                return new SchemeVector(((Quantity) v1).indexValue(),
                                        v2);
            case MAKESTRING:
                char newStr[]=new char[((Quantity) v1).indexValue()];
                char fillchar=SchemeCharacter.charValue(v2);
                for (int i=0; i<newStr.length; i++) {
                    newStr[i]=fillchar;
                }
                return new SchemeString(newStr);
            case STRINGAPPEND:
                SchemeString s1 = (SchemeString) v1;
                SchemeString s2 = (SchemeString) v2;
                StringBuffer sbuf = new StringBuffer(s1.length() + s2.length());
                s1.appendTo(sbuf);
                s2.appendTo(sbuf);
                return new SchemeString(sbuf.toString());
            case MAKERECTANGULAR:
                return Quantity.valueOf((Quantity) v1,
                                        (Quantity) v2);
            case ASHL: return Quantity.valueOf(((Quantity) v1).integer()
                                               .shiftLeft(((Quantity) v2)
                                                          .indexValue()));
            case ASHR: return Quantity.valueOf(((Quantity) v1).integer()
                                               .shiftRight(((Quantity) v2)
                                                           .indexValue()));
            case MAKECONFIGPARAM:
                return new ConfigParameter(SchemeString.asString(v1), v2);
            case LIST: return list(v1, v2);
            case LT:
                return SchemeBoolean.get(((Quantity) v1).comp((Quantity) v2,-1));
            case GRT:
                return SchemeBoolean.get(((Quantity) v1).comp((Quantity) v2,1));
            case INTERN:
                InternedValue iv = InternedValue.intern((Symbol) v1, v2);
                if (iv == null) {
                    throwPrimException(liMessage(SISCB, "internconflict",
                                                 v1.toString(),
                                                 v2.synopsis()));
                }
                return new Values(new Value[] { iv.getName(), iv.getValue() });
            default:
                throwArgSizeException();
                return VOID;
            }
        }

        public final Value apply(Value v1, Value v2, Value v3) 
            throws ContinuationException {      
            switch(id) {
            case CHAREQUAL:
                return SchemeBoolean.get(SchemeCharacter.charValue(v1) == SchemeCharacter.charValue(v2) &&
                             SchemeCharacter.charValue(v2) == SchemeCharacter.charValue(v3));
            case STRINGAPPEND:
                SchemeString s1 = (SchemeString) v1;
                SchemeString s2 = (SchemeString) v2;
                SchemeString s3 = (SchemeString) v3;
                StringBuffer sbuf = new StringBuffer(s1.length() + s2.length() + s3.length());
                s1.appendTo(sbuf);
                s2.appendTo(sbuf);
                s3.appendTo(sbuf);
                return new SchemeString(sbuf.toString());
            case STRINGEQUAL:
                return SchemeBoolean.get(((SchemeString) v1).valueEqual((SchemeString) v2) &&
                             ((SchemeString) v2).valueEqual((SchemeString) v3));
            case LIST: return list(v1, v2, v3);
            case ADD: return ((Quantity) v1).add((Quantity) v2).add((Quantity) v3);
            case MUL: return ((Quantity) v1).mul((Quantity) v2).mul((Quantity) v3);
            case SUB: return ((Quantity) v1).sub((Quantity) v2).sub((Quantity) v3);
            case NEQ: 
                Quantity q2=(Quantity) v2;
                return SchemeBoolean.get(((Quantity) v1).comp(q2,0) &&
                             q2.comp((Quantity) v3,0));
            case LT:
                q2=(Quantity) v2;
                return SchemeBoolean.get(((Quantity) v1).comp(q2,-1) &&
                             q2.comp((Quantity) v3,-1));
            case GRT:
                q2=(Quantity) v2;
                return SchemeBoolean.get(((Quantity) v1).comp(q2,1) &&
                             q2.comp((Quantity) v3,1));
            case DIV: 
                return ((Quantity) v1).div(((Quantity) v2).mul((Quantity) v3));
            case EQ: return SchemeBoolean.get(v1 == v2 && v2 == v3);
            case EQV: return SchemeBoolean.get(v1.eqv(v2));
            case EQUAL:
                return SchemeBoolean.get(v1.valueEqual(v2) && v2.valueEqual(v3));
            default:
                throwArgSizeException();
                return VOID;
            }
        }

        public final Value apply(Value v[]) throws ContinuationException {
            int vls=v.length;
            Quantity quantity=null;
            switch (id) {
            case LIST: return valArrayToList(v,0,vls);
            case ADD:
                int x=vls-1;
                quantity=(Quantity) v[x];
                while (--x >= 0) 
                    quantity=quantity.add((Quantity) v[x]);
                return quantity;
            case MUL:
                x=vls-1;
                quantity=(Quantity) v[x];
                while (--x >= 0) 
                    quantity=quantity.mul((Quantity) v[x]);
                return quantity;
            case SUB: 
                quantity=(Quantity) v[0];
                for (int i=1; i<vls; i++) {
                    quantity=quantity.sub((Quantity) v[i]);
                }
                return quantity;
            case NEQ:
                quantity=(Quantity) v[0];
                for (int i=vls-1; i>0; i--) {
                    if (!quantity.comp((Quantity) v[i], 0)) return FALSE;
                }
                return TRUE;
            case LT:
                quantity=(Quantity) v[0];
                for (int i=1; i<vls; i++) {
                    Quantity q=(Quantity) v[i];
                    if (!quantity.comp(q, -1)) return FALSE;
                    quantity=q;
                }
                return TRUE;
            case GRT:
                quantity=(Quantity) v[0];
                for (int i=1; i<vls; i++) {
                    Quantity q=(Quantity) v[i];
                    if (!quantity.comp(q, 1)) return FALSE;
                    quantity=q;
                }
                return TRUE;
            case DIV: 
                x=vls-1;
                quantity=(Quantity) v[x];
                while (--x >= 1) 
                    quantity=quantity.mul((Quantity) v[x]);
                return ((Quantity) v[0]).div(quantity);
            case STRINGAPPEND:
                StringBuffer sbuf = new StringBuffer();
                for (int i=0; i<vls; i++) {
                    ((SchemeString) v[i]).appendTo(sbuf);
                }
                return new SchemeString(sbuf.toString());
            case STRINGEQUAL:
                SchemeString str = (SchemeString) v[0];
                for (int i=1; i<vls; i++) {
                    SchemeString s = (SchemeString) v[i];
                    if (!str.valueEqual(s)) return FALSE;
                }
                return TRUE;
            case CHAREQUAL:
                char character = SchemeCharacter.charValue(v[0]);
                for (int i=1; i<vls; i++) {
                    char c = SchemeCharacter.charValue(v[i]);
                    if (!(character == c)) return FALSE;
                }
                return TRUE;
            default:
                throwArgSizeException();
            }
            return VOID;
        }        
    }    
    
    /**
     * The Complex procedures either have a side effect, or require the
     * interpreter to execute
     */
    public static class Complex extends IndexedProcedure {
        public Complex() {}
     
        Complex(int id) {
            super(id);
        }

        public final Value doApply(Interpreter r) throws ContinuationException {
            return doApply(r, r.vlr);
        }

        public final Value doApply(Interpreter r, Value[] vlr) 
            throws ContinuationException {
            final int vls = vlr.length;
            SIZESWITCH: switch (vls) {
            case 0:
                switch (id) {
                case CLASSPATHEXTENSION:
                    URL[] urls = r.dynenv.getClassPath();
                    Pair p = EMPTYLIST;
                    for (int i=urls.length-1; i>=0; i--) {
                        p = new Pair(new SchemeString(urls[i].toString()), p);
                    }
                    return p;
                case COMPACTSTRINGREP: return SchemeBoolean.get(SchemeString.compactRepresentation);
                case CURRENTWIND: return r.dynenv.wind;
                case GENSYM: 
                    long unv=r.tctx.nextUnique();
                    return Symbol.intern(GENSYM_MAGIC_PREFIX+base64encode(unv));
                case INTERACTIONENVIRONMENT:
                    return r.tpl.asValue();
                case SISCINITIAL: 
                    try {
                        return new MemorySymEnv(r.lookupContextEnv(Util.SISC_SPECIFIC));
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throwPrimException(liMessage(SISCB, "nosiscspecificenv"));
                    }
                default:
                    break SIZESWITCH;
                }
            case 1:
                switch (id) {
                case CLASSPATHEXTENSIONAPPEND:
                    for (Pair p = (Pair) vlr[0]; p != EMPTYLIST; p = (Pair) p.cdr()) {
                        r.dynenv.extendClassPath(url(p.car()));
                    }
                    return VOID;
                case SEALIMMUTABLEPAIR:
                    ((ImmutablePair) vlr[0]).makeImmutable();
                    return VOID;
                case SEALIMMUTABLEVECTOR:
                    ((ImmutableVector) vlr[0]).makeImmutable();
                    return VOID;
                case COMPACTSTRINGREP:
                    SchemeString.compactRepresentation=SchemeBoolean.toBoolean(vlr[0]);
                    return VOID;
                case NUMBER2STRING:
                    return new SchemeString(((Quantity) vlr[0]).toString());
                case GETENVIRONMENT:
                    try {
                        return r.getCtx().lookupContextEnv((Symbol) vlr[0]).asValue();
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throwPrimException(liMessage(SISCB, "noenv", vlr[0].synopsis()));
                        return VOID;
                    }
                case PARENTENVIRONMENT:
                    SymbolicEnvironment env=(SymbolicEnvironment) vlr[0];
                    SymbolicEnvironment parent=env.getParent();
                    if (parent == null) return FALSE;
                    else return parent.asValue();
                case GETSIDECAR:
                    return r.tpl.getSidecarEnvironment((Symbol) vlr[0]).asValue();
                case GETENV:
                    String str = r.getCtx().getProperty(SchemeString.asString(vlr[0]));
                    if (str == null) {
                        return FALSE;
                    } else {
                        return new SchemeString(str);
                    }
                case GENSYM: 
                    long unv=r.tctx.nextUnique();
                    return Symbol.intern(GENSYM_MAGIC_PREFIX+base64encode(unv));
                case COMPILE:
                    return new Closure(false,
                                       (short)0, 
                                       r.compile(vlr[0]),
                                       ZV,
                                       new int[0]);
                case CALLEC:
                    Value kproc=vlr[0];
                    r.setupTailCall(CALLEC_APPEVAL, r.captureEscapingContinuation());
                    return kproc;
                case CALLCC:
                    kproc=vlr[0];
                    r.setupTailCall(CALLCC_APPEVAL, r.captureContinuation());
                    return kproc;
                case CALLFC:
                    kproc=vlr[0];
                    r.setupTailCall(CALLFC_APPEVAL, r.fk.capture(r));
                    return kproc;
                case CURRENTWIND:
                    r.dynenv.wind = vlr[0];
                    return VOID;
                case LOADNL:
                    try {
                        Class clazz=Class.forName(SchemeString.asString(vlr[0]), true, Util.currentClassLoader());
                        return (NativeLibrary)clazz.newInstance();
                    } catch (Exception e) {
                        throwPrimException(e.getMessage());
                    }
                case GETPROP:
                    int loc=r.tpl.getLoc((Symbol) vlr[0]);
                    if (loc==-1) return FALSE;
                    else return r.tpl.lookup(loc); 
                case STRING2NUMBER:
                    String st=SchemeString.asString(vlr[0]);
                    try {
                        return (Quantity)r.dynenv.parser.nextExpression(new PushbackReader(new StringReader(st)));
                    } catch (ClassCastException cce) {
                        return FALSE;
                    } catch (NumberFormatException nf) {
                        return FALSE;
                    } catch (IOException e) {
                        return FALSE;
                    }
                case NLBINDINGNAMES:
                    Value[] va=((NativeLibrary) vlr[0]).getLibraryBindingNames(r);
                    return valArrayToList(va,0,va.length);        
                case INTERACTIONENVIRONMENT:
                    Value last = r.getCtx().toplevel_env.asValue();
                    r.getCtx().toplevel_env=(SymbolicEnvironment) vlr[0];
                    return last;
                case REPORTENVIRONMENT:
                    if (FIVE.equals((Quantity) vlr[0]))
                        try {
                            return new MemorySymEnv(r.lookupContextEnv(Util.REPORT));
                        } catch (ArrayIndexOutOfBoundsException e) {
                            throwPrimException(liMessage(SISCB, "noreportenv"));
                        }
                    else throwPrimException(liMessage(SISCB, "unsupportedstandardver"));
                case NULLENVIRONMENT:
                    switch (((Quantity) vlr[0]).indexValue()) {
                    case 5:
                        MemorySymEnv ae = new MemorySymEnv();
                        sisc.compiler.Compiler.addSpecialForms(ae);
                        return ae;
                    case 0:
                        return new MemorySymEnv();
                    default:
                        throwPrimException(liMessage(SISCB, "unsupportedstandardver"));
                        return VOID;
                    }
                default:
                    break SIZESWITCH;
                }
            case 2:
                switch (id) {
                case NLBINDING:
                    return ((NativeLibrary) vlr[0]).getBindingValue(r, (Symbol) vlr[1]);
                case COMPILE:
                    return new Closure(false,
                                       (short)0, 
                                       r.compile(vlr[0], (SymbolicEnvironment) vlr[1]),
                                       ZV,
                                       new int[0]);
                case WITHENVIRONMENT:
                    Procedure thunk=(Procedure) vlr[1];
                    r.tpl=(SymbolicEnvironment) vlr[0];
                    r.setupTailCall(WITHENV_APPEVAL, ZV);
                    return thunk;
                case WITHFC:
                    Procedure proc=(Procedure) vlr[1];
                    Procedure ehandler=(Procedure) vlr[0];
                    r.setFailureContinuation(new ApplyValuesContEval(ehandler));
                    r.setupTailCall(WITHFC_APPEVAL, ZV);
                    return proc;
                case CALLWITHVALUES:
                    Procedure producer=(Procedure) vlr[0];
                    Procedure consumer=(Procedure) vlr[1];
                    r.pushExpr(new ApplyValuesContEval(consumer));
                    r.setupTailCall(CALLWITHVALUES_APPEVAL, ZV);
                    return producer;
                case GETPROP:
                    Value ret = null;
                    if (vlr[1] instanceof SymbolicEnvironment) {
                        ret = ((SymbolicEnvironment) vlr[1]).lookup((Symbol) vlr[0]);
                    } else {
                        ret = r.tpl.getSidecarEnvironment(
                                 (Symbol) vlr[1]).lookup((Symbol) vlr[0]);
                    }
                    return (ret == null) ? FALSE : ret;
                case REMPROP:
                    if (vlr[1] instanceof SymbolicEnvironment) {
                        ((SymbolicEnvironment) vlr[1]).undefine((Symbol) vlr[0]);
                    } else {
                        r.tpl.getSidecarEnvironment((Symbol) vlr[1]).undefine((Symbol) vlr[0]); 
                    }
                    return VOID;
                case PUTPROP:
                    r.tpl.define((Symbol) vlr[0], vlr[1]);
                    return VOID;
                case SETBOX:
                    try {
                        ((Box) vlr[0]).set(vlr[1]);
                    } catch (ImmutableException e) {
                        throwPrimException(liMessage(SISCB, "isimmutable", "box",
                                                     vlr[0].synopsis()));
                    }
                    return VOID;
                case SETCAR:
                    truePair(vlr[0]).setCar(vlr[1]);
                    return VOID;
                case SETCDR:
                    truePair(vlr[0]).setCdr(vlr[1]);
                    return VOID;
                case SETENVIRONMENT:
                    r.getCtx().defineContextEnv((Symbol) vlr[0], (SymbolicEnvironment) vlr[1]);
                    return VOID;
                case SIGHOOK:
                    SignalHook.addHandler(SchemeString.asString(vlr[0]), (Procedure) vlr[1], r.dynenv);
                    return VOID;
                case SIGUNHOOK:
                    SignalHook.removeHandler(SchemeString.asString(vlr[0]), (Procedure) vlr[1], r.dynenv);
                    return VOID;                    
                case GETSIDECAR:
                    return ((SymbolicEnvironment) vlr[1]).getSidecarEnvironment((Symbol) vlr[0]).asValue();
                case STRING2NUMBER:
                    try {
                        int radix=((Quantity) vlr[1]).indexValue();
                        if (r.dynenv.parser.lexer.strictR5RS &&
                            !(radix==10 || radix == 16 || radix == 2 ||
                              radix==8))
                            throwPrimException(liMessage(SISCB, "invalidradix"));
                        return (Quantity)r.dynenv.parser.nextExpression(new PushbackReader(new StringReader(SchemeString.asString(vlr[0]))), radix, 0);
                    } catch (NumberFormatException nf) {
                        return FALSE;
                    } catch (IOException e) {
                        return FALSE;
                    }
                case NUMBER2STRING:
                    int radix=((Quantity) vlr[1]).indexValue();
                    if (r.dynenv.parser.lexer.strictR5RS &&
                        !(radix==10 || radix == 16 || radix == 2 ||
                          radix==8))
                        throwPrimException(liMessage(SISCB, "invalidradix"));
                    return new SchemeString(((Quantity) vlr[0]).toString(radix));
                case STRINGFILL:
                    SchemeString st=(SchemeString) vlr[0];
                    char c=SchemeCharacter.charValue(vlr[1]);
                    for (int i=0; i<st.length(); i++)
                        st.set(i, c);
                    return VOID;
                case VECTORFILL:
                    ((SchemeVector) vlr[0]).fill(vlr[1]);
                    return VOID;
                default:
                    break SIZESWITCH;
                }
            case 3:
                switch(id) {
                case STRINGSET:
                    int index=((Quantity) vlr[1]).indexValue();
                    try {
                        ((SchemeString) vlr[0]).set(index, SchemeCharacter.charValue(vlr[2]));
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throwPrimException(liMessage(SISCB, "indexoob", 
                                                     new Object[] {
                                                         new Integer(index),
                                                         vlr[0].synopsis()}));
                    }
                    return VOID;
                case VECTORSET:
                    index=((Quantity) vlr[1]).indexValue();
                    try {
                        ((SchemeVector) vlr[0]).set(index,vlr[2]);
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throwPrimException(liMessage(SISCB, "indexoob", 
                                                     new Object[] {
                                                         new Integer(index),
                                                         vlr[0].synopsis()}));
                    }
                    return VOID;
                case GETPROP:
                    Value ret = null;
                    if (vlr[1] instanceof SymbolicEnvironment) {
                        ret = ((SymbolicEnvironment) vlr[1]).lookup((Symbol) vlr[0]);
                    } else {
                        ret = r.tpl.getSidecarEnvironment(
                              (Symbol) vlr[1]).lookup((Symbol) vlr[0]);
                    }
                    return (ret == null) ? vlr[2] : ret;
                case PUTPROP:
                    Symbol lhs=(Symbol) vlr[0];
                    Value rhs=vlr[2];
                    SymbolicEnvironment env;
                    if (vlr[1] instanceof SymbolicEnvironment) {
                        env=(SymbolicEnvironment)vlr[1];
                    } else {
                        env=r.tpl.getSidecarEnvironment((Symbol)vlr[1]);
                    }
                    updateName(rhs, lhs);
                    env.define(lhs, rhs);               
                    return VOID;
                }
            }
            
            switch (id) {
            case APPLY:
                Procedure proc=(Procedure) vlr[0];
                int l = vls-2;

                Pair args=(Pair) vlr[l+1];
                Value newvlr[] = r.createValues(l+length(args));
                
                int j;
                for (j=0; j < l; j++) {
                    newvlr[j] = vlr[j+1];
                }
                for (; args != EMPTYLIST; args = (Pair)args.cdr()) {
                    newvlr[j++] = args.car();
                }
                r.setupTailCall(APPLY_APPEVAL, newvlr);
                return proc;
            default:
                throwArgSizeException();
            }
            return VOID;
        }
    }


    // next: 149, {145}
    static final int ACOS = 23,
        ADD = 114,
        APPLY = 121,
        ASHL = 102,
        ASHR = 103,
        ASIN = 22,
        ATAN = 93,
        BOOLEANQ = 32,
        BOX = 56,
        BOXQ = 58,
        CALLCC = 54,
        CALLEC = 126,
        CALLFC = 55,
        CALLWITHVALUES = 106,
        CAR = 15,
        CDR = 16,
        CEILING = 49,
        CHAR2INTEGER = 43,
        CHARACTERQ = 30,
        CHAREQUAL = 140,
        CIRCULARQ = 62,
        CLASSPATHEXTENSION = 142,
        CLASSPATHEXTENSIONAPPEND = 143,
        COMPACTSTRINGREP = 7,
        COMPILE = 144,
        COMPLEXQ = 37,
        CONS = 85,
        CONSIMMUTABLE = 131,
        COS = 20,
        CURRENTWIND = 70,
        DENOMINATOR = 67,
        DIV = 115,
        ENVIRONMENTQ = 34,
        EQ = 83,
        EQUAL = 86,
        EQV = 84,
        EXACT2INEXACT = 46,
        EXACTQ = 38,
        EXP = 25,
        EXPTYPE = 129,
        FLOOR = 48,
        GCD = 92,
        GENSYM = 0,
        GENSYMQ = 137,
        GETSIDECAR = 124,
        PARENTENVIRONMENT=148,
        GETENV = 123,
        GETENVIRONMENT = 18,
        GETPROP = 109,
        GRT = 118,
        HASHCODE = 136,
        IMAGPART = 69,
        IMMUTABLEPAIRQ = 132,
        IMMUTABLEVECTORQ = 133,
        INEXACT2EXACT = 47,
        INEXACTQ = 39,
        INTEGER2CHAR = 52,
        INTEGERQ = 36,
        INTERACTIONENVIRONMENT = 1,
        INTERN = 138,
        LCM = 91,
        LENGTH = 59,
        LIST = 120,
        LIST2VECTOR = 44,
        LOADNL = 77,
        LOG = 24,
        LT = 117,
        MAKECHILDENVIRONMENT = 147,       
        MAKEPARAM = 63,
        MAKENATIVEPARAM = 12,
        MAKECONFIGPARAM = 122,
        MAKERECTANGULAR = 101,
        MAKESTRING = 99,
        MAKEVECTOR = 65,
        MAKEIMMUTABLEVECTOR = 130,
        MAXFLOATPRECISION = 9,
        MINFLOATPRECISION = 10,
        MUL = 11,
        NEQ = 116,
        NLBINDING = 104,
        NLBINDINGNAMES = 76,
        NLNAME = 74,
        NLVERSION = 75,
        NULLENVIRONMENT = 73,
        NULLQ = 14,
        NUMBER2STRING = 80,
        NUMBERQ = 27,
        NUMERATOR = 66,
        PAIRQ = 17,
        PARAMETERQ = 40,
        PERMITINTERRUPTS = 8,
        PORTQ = 141,
        PROCEDUREQ = 35,
        PUTPROP = 110,
        QUOTIENT = 90,
        REALPART = 68,
        REMAINDER = 89,
        REMPROP = 107,
        REPORTENVIRONMENT = 72,
        ROUND = 50,
        SEALIMMUTABLEPAIR = 134,
        SEALIMMUTABLEVECTOR = 135,
        SETBOX = 94,
        SETCAR = 87,
        SETCDR = 88,
        SETENVIRONMENT = 108,
        SIGHOOK = 127,
        SIGUNHOOK = 128,
        SIN = 19,
        SISCINITIAL = 2,
        SLEEP = 78,
        SQRT = 26,
        STRING2NUMBER = 79,
        STRING2SYMBOL = 42,
        STRING2UNINTERNEDSYMBOL = 71,
        STRINGAPPEND = 100,
        STRINGFILL = 98,
        STRINGLENGTH = 60,
        STRINGQ = 31,
        STRINGEQUAL = 139,
        STRINGREF = 95,
        STRINGSET = 111,
        SUB = 119,
        SYMBOL2STRING = 41,
        SYMBOLQ = 29,
        SYNTOKEN2STRING = 125,
        SYNTOKENQ = 6,
        SYSTIME = 3,
        TAN = 21,
        TIMEZONEOFFSET = 4,
        TRUNCATE = 51,
        UNBOX = 57,
        VECTOR2LIST = 45,
        VECTORFILL = 113,
        VECTORFINDLASTUNIQUE = 53,
        VECTORLENGTH = 61,
        VECTORQ = 28,
        VECTORREF = 96,
        VECTORSET = 112,
        VOIDQ = 33,
        WITHENVIRONMENT = 146,
        WITHFC = 105,
        _VOID = 5;
}
/*
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 * 
 * The Original Code is the Second Interpreter of Scheme Code (SISC).
 * 
 * The Initial Developer of the Original Code is Scott G. Miller. Portions
 * created by Scott G. Miller are Copyright (C) 2000-2007 Scott G. Miller. All
 * Rights Reserved.
 * 
 * Contributor(s): Matthias Radestock
 * 
 * Alternatively, the contents of this file may be used under the terms of the
 * GNU General Public License Version 2 or later (the "GPL"), in which case the
 * provisions of the GPL are applicable instead of those above. If you wish to
 * allow use of your version of this file only under the terms of the GPL and
 * not to allow others to use your version of this file under the MPL, indicate
 * your decision by deleting the provisions above and replace them with the
 * notice and other provisions required by the GPL. If you do not delete the
 * provisions above, a recipient may use your version of this file under either
 * the MPL or the GPL.
 */
