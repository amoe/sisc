package sisc.util;

import sisc.data.*;
import java.io.*;
import java.util.Collections;
import java.util.Set;
import java.util.HashSet;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.env.SymbolicEnvironment;
import sisc.util.ExpressionVisitor;

public class FreeReference implements ExpressionVisitee {

    private static Set allReferences =
        Collections.synchronizedSet(new HashSet());

    private Symbol sym;
    private SymbolicEnvironment senv;
    private transient int envLoc=-1;

    public FreeReference(Symbol sym, SymbolicEnvironment senv) {
        this.senv = senv;
        this.sym = sym;
        //allReferences.add(this);
    }

    public static FreeReference[] allReferences() {
        return (FreeReference[])allReferences.toArray(new FreeReference[] {});
    }

    public Symbol getName() {
        return sym;
    }

    public void resolve() throws UndefinedVarException {
        //this is an optimization that ensures we short-circuit
        //any DelegatingSymEnvs
        senv = (SymbolicEnvironment)senv.asValue();
        envLoc=senv.getLoc(sym);
        if (envLoc<0) throw new UndefinedVarException(sym.toString());
    }

    public Value getValue() throws UndefinedVarException {
        if (envLoc<0) resolve();
        return senv.lookup(envLoc);
    }

    public void setValue(Value v) throws UndefinedVarException {
        if (envLoc<0) resolve();
        senv.set(envLoc, v);
        Util.updateName(v, sym);
    }

    public void define(Value v) {
        senv.define(sym, v);
        Util.updateName(v, sym);
    }

    public Value express() {
        return Util.list(sym, (Value)senv);
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(sym);
        s.writeSymbolicEnvironment(senv);
    }

    public FreeReference() {}

    public void deserialize(Deserializer s) throws IOException {
        sym=(Symbol)s.readExpression();
        senv=s.readSymbolicEnvironment();
        envLoc=-1;
        //allReferences.add(this);
    }

    public boolean equals(Object o) {
        if (!(o instanceof FreeReference))
            return false;
        FreeReference e=(FreeReference)o;
        return sym.equals(e.sym) && senv.equals(e.senv);
    }

    public int hashCode() {
        return sym.hashCode() ^ senv.hashCode();
    }

    public boolean visit(ExpressionVisitor v) {
        return v.visit(sym) && v.visit(senv);
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
