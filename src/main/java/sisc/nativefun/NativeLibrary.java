package sisc.nativefun;

import sisc.data.*;
import sisc.interpreter.*;
import java.io.IOException;

import sisc.io.ValueWriter;

/**
 * A Native Library is a collection of bindings that can be imported
 * into any environment in SISC.  This API provides for enumerating
 * and fetching the bindings, as well as naming and versioning.
 */
public abstract class NativeLibrary extends Value
    implements java.io.Serializable, NamedValue {

    public abstract Symbol[] getLibraryBindingNames(Interpreter r);
    public abstract Value getBindingValue(Interpreter r, Symbol name) throws NoSuchMethodError;

    public abstract String getLibraryName();
    public abstract float getLibraryVersion();

    public void display(ValueWriter w) throws IOException {
        displayNamedOpaque(w, liMessage(SISCB, "nativelibrary"));
    }

    public void bindAll(Interpreter r, sisc.env.SymbolicEnvironment env) {
        Symbol[] syms=getLibraryBindingNames(r);
        for (int i=0; i<syms.length; i++) {
            env.define(syms[i], getBindingValue(r, syms[i]));
        }
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
