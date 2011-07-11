package sisc.nativefun;

import java.util.*;
import sisc.data.*;
import sisc.interpreter.*;

public abstract class IndexedLibraryAdapter extends NativeLibrary {
    static class Token {
        Class context;
        int id;
        
        public Token(Class context, int id) {
            this.context=context;
            this.id=id;
            
        }
    }
    protected HashMap bindings=new HashMap(0);

    public abstract Value construct(Object context, int id);

    protected void define(String s, int id) {
        define(s, null, id);
    }

    protected void define(String s, Class context, int id) {
        Symbol name=Symbol.get(s);
        bindings.put(name, new Token(context, id));
    }
    
    public String getLibraryName() {
        return getClass().getName();
    }

    public Symbol[] getLibraryBindingNames(Interpreter r) {
        Symbol[] slist=new Symbol[bindings.size()];
        int x=0;
        for (Iterator i=bindings.keySet().iterator(); 
             i.hasNext();) 
            slist[x++]=(Symbol)i.next();
        return slist;
    }


    public Value getBindingValue(Interpreter r, Symbol name) throws NoSuchMethodError {
        Token t=(Token)bindings.get(name);
        try {            
            Value v=construct(t.context, t.id);
            if (v instanceof NamedValue)
                v.setName(name);
           
         return v;
        } catch (NullPointerException n) {
            throw new NoSuchMethodError();
        }
    }
    
    public float getLibraryVersion() {
        return 0.0f;
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
