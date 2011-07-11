package sisc.env;

import sisc.data.*;

import java.io.IOException;

import sisc.io.ValueWriter;
import sisc.util.ExpressionVisitor;
import sisc.interpreter.Context;

public class DelegatingSymEnv extends Value
    implements SymbolicEnvironment, NamedValue {

    private SymbolicEnvironment delegee;

    protected SymbolicEnvironment getEnv() {
        if (delegee == null) {
            delegee = (SymbolicEnvironment)Context.currentAppContext().getExpression((Symbol)getName());
        }
        return delegee;
    }

    public DelegatingSymEnv() {}

    public DelegatingSymEnv(Symbol name) {
        setName(name);
    }

    public void display(ValueWriter w) throws IOException {
        displayNamedOpaque(w, "environment");
    }

    public void setParent(SymbolicEnvironment e) {
        getEnv().setParent(e);
    }
    
    public SymbolicEnvironment getParent() {
        return getEnv().getParent();
    }
    
    public SymbolicEnvironment getSidecarEnvironment(Symbol env) {
        return getEnv().getSidecarEnvironment(env);
    }

    public sisc.data.Value asValue() {
        return getEnv().asValue();
    }

    public void set(int envLoc, Value v) {
        getEnv().set(envLoc, v);
    }

    public int define(Symbol s, Value v) {
        return getEnv().define(s, v);
    }

    public int getLoc(Symbol s) {
        return getEnv().getLoc(s);
    }

    public Value lookup(Symbol s) {
        return getEnv().lookup(s);
    }

    public final Value lookup(int pi) {
        return getEnv().lookup(pi);
    }

    public void undefine(Symbol s) {
        getEnv().undefine(s);
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(delegee);
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
