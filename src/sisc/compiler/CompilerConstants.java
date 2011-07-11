package sisc.compiler;

import sisc.util.Util;
import sisc.data.Symbol;
import java.util.Map;
import java.util.HashMap;
import sisc.env.SymbolicEnvironment;

public abstract class CompilerConstants extends Util {
    
    //Various integer constants
    public static final int 
        SYNTACTIC_TOKEN_COUNT = 9,
        PROGRAM=0, APPLICATION=1, LAMBDA=2, _IF=3, BEGIN=4, QUOTE=5, SET=6, 
        DEFINE=7, MAKEANNOTATION=8, LETREC=9, UNKNOWN=-1, 
        REALTAIL=1;
    
    public static final Map SYNTACTIC_TOKENS=new HashMap(SYNTACTIC_TOKEN_COUNT);
    
    //Define the core syntactic constructs of the interpreter
    static final Syntax
        SYN_QUOTE  = syntax("quote", QUOTE),
        SYN_PROGRAM= syntax("program", PROGRAM),
        SYN_BEGIN  = syntax("begin", BEGIN),
        SYN_IF     = syntax("if", _IF),
        SYN_DEFINE = syntax("define", DEFINE),
        SYN_SET    = syntax("set!", SET), 
        SYN_LAMBDA = syntax("lambda", LAMBDA),
        SYN_LETREC = syntax("letrec", LETREC),
        //SYN_FIX = syntax("fix", FIX),
        SYN_ANNOT  = syntax("annotate", MAKEANNOTATION),
        SYN_UNKNOWN = syntax("unknown", UNKNOWN);
    
    
    static Syntax syntax(String name, int id) {
        Syntax s=new Syntax(id);
        s.setName(Symbol.get(name));
        SYNTACTIC_TOKENS.put(name, s);
        SYNTACTIC_TOKENS.put(new Integer(id), s);
        return s;
    }
 
    static void extendenv(SymbolicEnvironment env, String s, Syntax syn) {
        Symbol name=Symbol.get(s);
        env.define(name, syn);
    }       

    static final Symbol 
        VARNAME=Symbol.get("var-name"),
        PROCNAME=Symbol.get("proc-name"),
        _LETREC=Symbol.get("letrec");
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
