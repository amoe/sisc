package sisc.modules;

import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.env.SymbolicEnvironment;

public class R5RS extends NativeLibrary {

    public static Symbol[] bindingNames;

    private static String[] bindingStrings = new String[] {
        //core forms
        "lambda", "quote", "letrec", "if", "begin", "set!", "define",

        //procedures
        "*", "+", "-", "/", "<", "<=", "=", ">", ">=", "abs", "acos", "angle",
        "append", "apply", "asin", "assoc", "assq", "assv", "atan", "boolean?",
        "call-with-current-continuation", "call-with-input-file",
        "call-with-output-file", "call-with-values", "car", "cdr", "caar",
        "cadr", "cdar", "cddr", "caaar", "caadr", "cadar", "caddr", "cdaar",
        "cdadr", "cddar", "cdddr", "caaaar", "caaadr", "caadar", "caaddr",
        "cadaar", "cadadr", "caddar", "cadddr", "cdaaar", "cdaadr", "cdadar",
        "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr", "ceiling",
        "char->integer", "char-alphabetic?", "char-ci<=?", "char-ci<?",
        "char-ci=?", "char-ci>=?", "char-ci>?", "char-downcase",
        "char-lower-case?", "char-numeric?", "char-ready?", "char-upcase",
        "char-upper-case?", "char-whitespace?", "char<=?", "char<?", "char=?",
        "char>=?", "char>?", "char?", "close-input-port", "close-output-port",
        "complex?", "cons", "cos", "current-input-port", "current-output-port",
        "denominator", "display", "dynamic-wind", "eof-object?", "eq?", "equal?",
        "eqv?", "eval", "even?", "exact->inexact", "exact?", "exp", "expt",
        "floor", "for-each", "force", "gcd", "imag-part", "inexact->exact",
        "inexact?", "input-port?", "integer->char", "integer?",
        "interaction-environment", "lcm", "length", "list", "list->string",
        "list->vector", "list-ref", "list-tail", "list?", "load", "log",        
        "magnitude", "make-polar", "make-rectangular", "make-string",
        "make-vector", "map", "max", "member", "memq", "memv", "min", "modulo",
        "negative?", "newline", "not", "null-environment", "null?",
        "number->string", "number?", "numerator", "odd?", "open-input-file",
        "open-output-file", "output-port?", "pair?", "peek-char", "positive?",
        "procedure?", "quotient", "rational?", "rationalize", "read",
        "read-char", "real-part", "real?", "remainder", "reverse", "round",
        "scheme-report-environment", "set-car!", "set-cdr!", "sin", "sqrt",
        "string", "string->list", "string->number", "string->symbol",
        "string-append", "string-ci<=?", "string-ci<?", "string-ci=?",
        "string-ci>=?", "string-ci>?", "string-copy", "string-fill!",
        "string-length", "string-ref", "string-set!", "string<=?", "string<?",
        "string=?", "string>=?", "string>?", "string?", "substring",
        "symbol->string", "symbol?", "tan", "truncate", "values", "vector",
        "vector->list", "vector-fill!", "vector-length", "vector-ref",
        "vector-set!", "vector?", 
        "with-input-from-file", "with-output-to-file",
        "write", "write-char", "zero?",

        //Yes, I know, these shouldn't be here, but we have problems without them
        "make-promise", "$syntax-dispatch", "$sc-put-cte", "syntax-error"
        
        };

    static {
        bindingNames = new Symbol[bindingStrings.length];
        for (int i=0; i<bindingStrings.length; i++)
            bindingNames[i] = Symbol.get(bindingStrings[i]);
    }

    public String getLibraryName() {
        return "R5RS";
    }

    public float getLibraryVersion() {
        return 0.0f;
    }

    public R5RS() {}

    public Symbol[] getLibraryBindingNames(Interpreter r) {
        return bindingNames;
    }

    public Value getBindingValue(Interpreter r, Symbol name)
        throws NoSuchMethodError {
        SymbolicEnvironment env = r.lookupContextEnv(REPORT);
        return env.lookup(name);
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
