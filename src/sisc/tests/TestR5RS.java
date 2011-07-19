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
 * Marvin Sielenkemper
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

package sisc.tests;

import java.io.StringReader;
import java.io.PushbackReader;
import java.io.BufferedReader;
import java.io.IOException;

import junit.framework.TestCase;

import sisc.REPL;
import sisc.data.Procedure;

import sisc.data.Value;
import sisc.data.SchemeCharacterInputPort;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;

public class TestR5RS
    extends TestCase
{
    private Interpreter interpreter;


    public TestR5RS(String name)
    {
        super(name);
    }


    protected void setUp()
        throws ClassNotFoundException
    {
        interpreter = Context.enter();
    }

    protected void tearDown()
    {
        interpreter = null;
        System.gc();
    }


    private Value quote(String expression)
        throws IOException
    {
        return
            interpreter.
            dynenv.
            parser.
            nextExpression(
            		new PushbackReader(
            		new BufferedReader(
                        new StringReader(
                            expression
                        )
                    )
                )
            );
    }

    private Value eval(String expression)
        throws IOException, SchemeException
    {
        return interpreter.eval(quote(expression));
    }

    private void check(String in, String out)
        throws IOException, SchemeException
    {
        Value   value   = eval(in);
        boolean success = value.valueEqual(quote(out));

        if (!success)
        {
            System.out.println(
                "*** evaluation of ***\n" +
                in + '\n' +
                "*** returned ***\n" +
                value + '\n' +
                "*** expected was ***\n" +
                out + '\n' +
                "*** end ***"
            );
        }

        assertTrue(success);
    }


    // 4 Expressions

    // 4.1 Primitive expression types

    /// 4.1.1 Variable references
    public void test4_1_1()
        throws Exception
    {
        eval("(define x 28)");
        check("x", "28");
    }

    /// 4.1.2 Literal expressions
    public void test4_1_2()
        throws Exception
    {
        check("(quote a)"       , "a"       );
        check("(quote #(a b c))", "#(a b c)");
        check("(quote (+ 1 2))" , "(+ 1 2)" );

        check("'a"        , "a"        );
        check("'#(a b c)" , "#(a b c)" );
        check("'()"       , "()"       );
        check("'(+ 1 2)"  , "(+ 1 2)"  );
        check("'(quote a)", "(quote a)");
        check("''a"       , "(quote a)");

        check("'\"abc\"", "\"abc\"");
        check("\"abc\"" , "\"abc\"");
        check("'145932" , "145932" );
        check("145932"  , "145932" );
        check("'#t"     , "#t"     );
        check("#t"      , "#t"     );
    }

    public void test4_1_2a()
        throws Exception
    {
        try
        {
            eval("(set-car! '(1 . 2) 'a)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test4_1_2b()
        throws Exception
    {
        try
        {
            eval("(string-set! '\"abc\" 0 #\\b)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test4_1_2c()
        throws Exception
    {
        try
        {
            eval("(vector-set! '#(1 2 3) 0 'a)");
            fail();
        }
        catch (SchemeException e)
        { }
    }


    /// 4.1.3 Procedure calls

    public void test4_1_3a()
        throws Exception
    {
        check("(+ 3 4)"          , "7" );
        check("((if #f + *) 3 4)", "12");
    }

 public void failingtest4_1_3b()
  throws Exception
 {
  try
  {
   eval("()");
   fail();
  }
  catch (SchemeException e)
  { }
 }


    /// 4.1.4 Procedures

    public void test4_1_4()
        throws Exception
    {
        assertTrue(eval("(lambda (x) (+ x x))") instanceof Procedure);
        check("((lambda (x) (+ x x)) 4)", "8");

        eval(
            "(define reverse-subtract\n" +
            "  (lambda (x y) (- y x)))"
        );
        check("(reverse-subtract 7 10)", "3");

        eval(
            "(define add4\n" +
            "  (let ((x 4))\n" +
            "    (lambda (y) (+ x y))))"
        );
        check("(add4 6)", "10");
    }

    public void test4_1_4a()
        throws Exception
    {
        try
        {
            eval("(lambda (x y x) y)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test4_1_4b()
        throws Exception
    {
        check("((lambda x x) 3 4 5 6)", "(3 4 5 6)");
        check("((lambda (x y . z) z) 3 4 5 6)", "(5 6)");
    }


    /// 4.1.5 Conditionals

    public void test4_1_5()
        throws Exception
    {
        check("(if (> 3 2) 'yes 'no)", "yes");
        check("(if (> 2 3) 'yes 'no)", "no");
        check("(if (> 3 2)\n" +
              "    (- 3 2)\n" +
              "    (+ 3 2))",
              "1"
        );
    }


    /// 4.1.6 Assignments

    public void test4_1_6()
        throws Exception
    {
        eval("(define x 2)");
        check("(+ x 1)", "3");
        eval("(set! x 4)");
        check("(+ x 1)", "5");
    }


    // 4.2 Derived expression types

    /// 4.2.1 Conditionals

    public void no_test4_2_1()
        throws Exception
    {
        check(
            "(cond ((> 3 2) 'greater)\n" +
            "      ((< 3 2) 'less))",
            "greater"
        );
        check(
            "(cond ((> 3 3) 'greater)\n" +
            "      ((< 3 3) 'less))\n" +
            "      (else 'equal))",
            "equal"
        );
        check(
            "(cond ((assv 'b '((a 1) (b 2))) => cadr)\n" +
            "      (else #f))",
            "2"
        );
    }


    /// 4.2.2 Binding constructs

    public void test4_2_2_let()
        throws Exception
    {
        check(
            "(let ((x 2) (y 3))\n" +
            "  (* x y))",
            "6"
        );

        check(
            "(let ((x 2) (y 3))\n" +
            "  (let ((x 7)\n" +
            "        (z (+ x y)))\n" +
            "    (* z x)))",
            "35"
        );
    }

    public void failingtest4_2_2a_let()
        throws Exception
    {
        try
        {
            eval("(let ((x 1) (y x)) 0)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void failingtest4_2_2b_let()
        throws Exception
    {
        try
        {
            eval("(let ((x y) (y 1)) 0)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test4_2_2a_letstar()
        throws Exception
    {
        check(
            "(let ((x 2) (y 3))\n" +
            "  (let* ((x 7)\n" +
            "         (z (+ x y)))\n" +
            "    (* z x)))",
            "70"
        );
    }

 public void test4_2_2b_letstar()
  throws Exception
 {
  check(
   "(let* ((a 1) (b (+ a 1)) (c (+ a b)) (d (+ b c)) (e (+ c d)))" +
   "  (list a b c d e))",
   "(1 2 3 5 8)"
  );
 }

 public void test4_2_2c_letstar()
  throws Exception
 {
  check("(let* ((x 1) (y x)) y)", "1");
 }

 public void failingtest4_2_2d_letstar()
  throws Exception
 {
  try
  {
   eval("(let* ((x y) (y 1)) 0)");
   fail();
  }
  catch (SchemeException e)
  { }
 }

    public void test4_2_2_letrec()
        throws Exception
    {
        check(
            "(letrec ((even?\n" +
            "          (lambda (n)\n" +
            "            (if (zero? n)\n" +
            "                #t\n" +
            "                (odd? (- n 1)))))\n" +
            "         (odd?\n" +
            "          (lambda (n)\n" +
            "            (if (zero? n)\n" +
            "                #f\n" +
            "                (even? (- n 1))))))\n" +
            "  (even? 88))\n",
            "#t"
        );

        check("(letrec ((x (lambda () y)) (y (lambda () x))) 0)",  "0");
    }

    public void test4_2_2_common()
        throws Exception
    {
        check("(let ((x 1)) (let    () (define x 2) x) x)", "1");
        check("(let ((x 1)) (let*   () (define x 2) x) x)", "1");
        check("(let ((x 1)) (letrec () (define x 2) x) x)", "1");

        check("(let ((x 1)) (begin     (define x 2) x) x)", "2");
    }


    /// 4.2.3 Sequencing

    public void test4_2_3()
        throws Exception
    {
        eval("(define x 0)");
        check(
            "(begin (set! x 5)\n" +
            "       (+ x 1))",
            "6"
        );
    }


    /// 4.2.4 Iteration

    public void test4_2_4_named_let()
        throws Exception
    {
        check(
            "(let f ((x 3) (y 7))\n" +
            "  (if (= x 0) y (f (- x 1) y)))",
            "7"
        );
    }


    /// 4.2.5 Delayed evaluation

    /// 4.2.6 Quasiquotation

    public void test4_2_6()
        throws Exception
    {
        check("`(list ,(+ 1 2) 4)", "(list 3 4)");
        check(
            "(let ((name 'a)) `(list ,name ',name))",
            "(list a (quote a))"
        );
        check(
            "`(a ,(+ 1 2) ,@(list 4 5 6) b)",
            "(a 3 4 5 6 b)"
        );
        check(
            "`#(10 5 ,(+ 1 1) ,@(list 4 3) 8)",
            "#(10 5 2 4 3 8)"
        );
        check(
            "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)",
            "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"
        );
        check(
            "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))",
            "(a `(b ,x ,'y d) e))"
        );
    }


    /// 5.2 Definitions

    public void test5_2()
        throws Exception
    {
        try
        {
            eval("(define x 1 2 3)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test5_2a()
        throws Exception
    {
        try
        {
            eval("(set! x 1 2 3)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test5_2b()
        throws Exception
    {
        check(
            "(begin (define (f x) x x x x x) (f 1))",
            "1"
        );

        try {
            check(
                "(let ((a 1))"+
                "  (define (f x)"+
                "    (define b (+ a x))"+
                "    (define a 5)"+
                "    (+ a b))"+
                "  (f 10))",
                "20"
            );
        }
        catch (SchemeException e)
        { }
    }


    /// 5.3 Syntax definitions

//  public void test5_3()
//      throws Exception
//  {
//      eval(
//          "(define-syntax static-cons " +
//          "  (lambda (def-env use-env . args)" +
//          "    (cons " +
//          "      use-env " +
//          "      (list 'quote (apply cons args)))))"
//      );
//      check(
//          "(static-cons a b)",
//          "(a . b)"
//      );
//  }


    /// 6.1 Equivalence predicates

    public void test6_1a()
        throws Exception
    {
        // eqv?
        check("(eqv? 'a 'a)", "#t");
        check("(eqv? 'a 'b)", "#f");
        check("(eqv? 2 2)", "#t");
        check("(eqv? '() '())", "#t");
        check("(eqv? 100000000 100000000)", "#t");
        check("(eqv? (cons 1 2) (cons 1 2))", "#f");
        check("(eqv? (lambda () 1) (lambda () 2))", "#f");
        check("(eqv? #f 'nil)", "#f");
        check("(let ((p (lambda (x) x))) (eqv? p p))", "#t");

        eval(
            "(define gen-counter " +
            "  (lambda () " +
            "    (let ((n 0)) " +
            "      (lambda () " +
            "        (set! n (+ n 1)) " +
            "        n))))"
        );

        check("(let ((g (gen-counter))) (eqv? g g))", "#t");
        check("(eqv? (gen-counter) (gen-counter))", "#f");
        
        check("(let ((x '(a))) (eqv? x x))", "#t");
    }

    public void test6_1b()
        throws Exception
    {
        // eq?
        check("(eq? 'a 'a)", "#t");
        check("(eq? (list 'a) (list 'a))", "#f");
        check("(eq? '() '())", "#t");
        check("(eq? car car)", "#t");
        check("(let ((x '(a))) (eq? x x))", "#t");
        check("(let ((x '#())) (eq? x x))", "#t");        
        check("(let ((p (lambda (x) x))) (eq? p p))", "#t");
    }

    public void test6_1c()
        throws Exception
    {
        // equal?
        check("(equal? 'a 'a)", "#t");
        check("(equal? '(a) '(a))", "#t");
        check("(equal? '(a (b) c) '(a (b) c))", "#t");
        check("(equal? \"abc\" \"abc\")", "#t");
        check("(equal? 2 2)", "#t");
        check("(equal? (make-vector 5 'a) (make-vector 5 'a))", "#t");
    }


    // 6.2 Numbers


    /// 6.2.5 Numerical operations
    
    public void testNumbers()
        throws Exception
    {
        check("(+ 3 4)", "7");
        check("(+ 3)", "3");
        check("(+)", "0");
        check("(* 4)", "4");
        check("(*)", "1");


        check("(+)", "0");
        check("(*)", "1");

        check("(- 1)", "-1");
        check("(/ 1)",  "1");

        check("(+ 3 3)", "6");
        check("(- 3 3)", "0");
        check("(* 3 3)", "9");
        check("(/ 3 3)", "1");

        check("(+  1 2 3 4)", "10");
        check("(-  1 2 3 4)", "-8");
        check("(*  1 2 3 4)", "24");
        check("(/ 24 2 3 4)",  "1");
    }


    // 6.3 Other data types

    // 6.3.1 Booleans

    /// 6.3.2 Pairs and lists

    public void test6_3_2()
        throws Exception
    {
        // pair?
        check("(pair? '(a . b))", "#t");
        check("(pair? '(a b c))", "#t");
        check("(pair? '())", "#f");
        check("(pair? '#(a b))", "#f");

        // cons
        check("(cons 'a '())", "(a)");
        check("(cons '(a) '(b c d))", "((a) b c d)");
        check("(cons \"a\" '(b c))", "(\"a\" b c)");
        check("(cons 'a '3)", "(a . 3)");
        check("(cons '(a b) 'c)", "((a b) . c)");

        // car
        check("(car '(a b c))", "a");
        check("(car '((a) b c d))", "(a)");
        check("(car '(1 . 2))", "1");
        try
        {
            eval("(car '())");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test6_3_2a()
        throws Exception
    {
        // cdr
        check("(cdr '((a) b c d))", "(b c d)");
        check("(cdr '(1 . 2))", "2");
        try
        {
            eval("(cdr '())");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test6_3_2b()
        throws Exception
    {
        eval("(define (f) (list 'not-a-constant-list))");
        eval("(define (g) '(constant-list))");

        // set-car!
        eval("(set-car! (f) 3)");
        try
        {
            eval("(set-car! (g) 3)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test6_3_2c()
        throws Exception
    {
        eval("(define (f) (list 'not-a-constant-list))");
        eval("(define (g) '(constant-list))");

        // set-cdr!
        eval("(set-cdr! (f) 3)");
        try
        {
            eval("(set-cdr! (g) 3)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test6_3_2d()
        throws Exception
    {
        // list?
        check("(list? '(a b c))", "#t");
        check("(list? '())", "#t");
        check("(list? '(a . b))", "#f");

        // list
        check("(list 'a (+ 3 4) 'c)", "(a 7 c)");
        check("(list)", "()");

        // length
        check("(length '(a b c))", "3");
        check("(length '(a (b) (c d e)))", "3");
        check("(length '())", "0");

        // append
        check("(append '(x) '(y))", "(x y)");
        check("(append '(a) '(b c d))", "(a b c d)");
        check("(append '(a (b)) '((c)))", "(a (b) (c))");

        check("(append '(a b) '(c . d))", "(a b c . d)");
        check("(append '() 'a)", "a");

        // reverse
        check("(reverse '(a b c))", "(c b a)");
        check("(reverse '(a (b c) d (e (f))))", "((e (f)) d (b c) a)");

        // memq/memv/member
        check("(memq 'a '(a b c))", "(a b c)");
        check("(memq 'b '(a b c))", "(b c)");
        check("(memq 'a '(b c d))", "#f");
        check("(memq   (list 'a) '(b (a) c))", "#f");
        check("(member (list 'a) '(b (a) c))", "((a) c)");
        check("(memv '101 '(100 101 102))", "(101 102)");

        // assq/assv/assoc
        eval("(define e '((a 1) (b 2) (c 3)))");
        check("(assq 'a e)", "(a 1)");
        check("(assq 'b e)", "(b 2)");
        check("(assq 'd e)", "#f");
        check("(assq  (list 'a) '(((a)) ((b)) ((c))))", "#f");
        check("(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))");
        check("(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)");
        check("(assq 'b '((a . 1) (b . 2)))", "(b . 2)");
    }


    /// 6.3.3 Symbols

    public void test6_3_3()
        throws Exception
    {
        // symbol?
        check("(symbol? 'foo)"        , "#t");
        check("(symbol? (car '(a b)))", "#t");
        check("(symbol? \"bar\")"     , "#f");
        check("(symbol? 'nil)"        , "#t");
        check("(symbol? '())"         , "#f");
        check("(symbol? #f)"          , "#f");

        // symbol->string
        // string->symbol
        check("(symbol->string 'flying-fish)", "\"flying-fish\"");
        check("(symbol->string 'Martin)"     , "\"martin\"");
        check("(symbol->string (string->symbol \"Malvia\"))", "\"Malvia\"");
    }


    /// 6.3.5 Strings

    public void test6_3_5()
        throws Exception
    {
        eval("(define (f) (make-string 3 #\\*))");
        eval("(define (g) \"***\"))");

        eval("(string-set! (f) 0 #\\?)");

        try
        {
            eval("(string-set! (g) 0 #\\?)");
            fail();
        }
        catch (SchemeException e)
        { }
    }

    public void test6_3_5a()
        throws Exception
    {
        try
        {
            eval("(string-set! (symbol->string 'immutable) 0 #\\?)");
            fail();
        }
        catch (SchemeException e)
        { }
    }


    /// 6.3.6 Vectors

    public void test6_3_6()
        throws Exception
    {
        check("(vector-ref '#(1 1 2 3 5 8 13 21) 5)", "8");
        check("(vector 'a 'b 'c)", "#(a b c)");

        check("(vector-length '#(1 2 3 4))", "4");


    }


    /// 6.4 Control features

    public void test6_4_procedureq()
        throws Exception
    {
        check("(procedure?  car)", "#t");
        check("(procedure? 'car)", "#f");
        check("(procedure?  (lambda (x) (* x x)))", "#t");
        check("(procedure? '(lambda (x) (* x x)))", "#f");
        check("(call-with-current-continuation procedure?)", "#t");
    }

    public void test6_4_apply()
        throws Exception
    {
        check("(apply + (list 3 4))", "7");
        check("(apply + 1 2 '(3 4))", "10");
        
        check(
            "(let ((l (list 1 2)))" +
            "  (apply" +
            "    (lambda x (set-car! x 3))" +
            "    l)" +
            "  l)",
            "(1 2)"
        );
    }

    public void test6_4_map()
        throws Exception
    {
        check(
            "(map + '(1 2 3) '(4 5 6))",
            "(5 7 9)"
        );
    }

    public void test6_4_for_each()
        throws Exception
    {
        check(
            "(let ((v (make-vector 5)) " +
            "      (n 0)) " +
            "  (for-each " +
            "    (lambda (x y) " +
            "      (vector-set! v n (+ x y)) " +
            "      (set! n (+ n 1))) " +
            "    '(0 1 2 3 4) " +
            "    '(1 1 2 3 5)) " +
            "  v)",
            "#(1 2 4 6 9)"
        );
    }

    public void test6_4_force()
        throws Exception
    {
        check(
            "(force (delay (+ 1 2)))",
            "3"
        );

        check(
            "(let ((p (delay (+ 1 2)))) (list (force p) (force p)))",
            "(3 3)"
        );

        eval(
            "(begin" +
            "  (define count 0)" +
            "  (define x 'foo)" +
            "  (define p" +
            "    (delay" +
            "      (begin" +
            "        (set! count (+ count 1))" +
            "        (if (> count x)" +
            "          count" +
            "          (force p)))))" +
            "  (define x 5))"
        );

        check("(force p)", "6");
        eval("(set! x 10)");
        check("(force p)", "6");
    }

    public void test6_4_callcc()
        throws Exception
    {
        check(
            "(let ((path '())\n"+
            "      (c #f))\n"+
            "  (let ((add (lambda (s)\n"+
            "               (set! path (cons s path)))))\n"+
            "    (dynamic-wind\n"+
            "      (lambda () (add 'connect))\n"+
            "      (lambda ()\n"+
            "        (add (call-with-current-continuation\n"+
            "               (lambda (c0)\n"+
            "                 (set! c c0)\n"+
            "                 'talk1))))\n"+
            "      (lambda () (add 'disconnect)))\n"+
            "    (if (< (length path) 4)\n"+
            "        (c 'talk2)\n"+
            "        (reverse path))))",
            "(connect talk1 disconnect" +
            " connect talk2 disconnect)"
        );
    }


    /// 6.5 Eval

    public void test6_5()
        throws Exception
    {
        check("(eval '(* 7 3) (scheme-report-environment 5))", "21");
        check(
            "(let ((f (eval '(lambda (f x) (f x x))\n"+
            "       (null-environment 5))))\n"+
            "  (f + 10))",
            "20"
        );
    }
}
