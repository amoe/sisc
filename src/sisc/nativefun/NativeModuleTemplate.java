package sisc.nativefun;

import sisc.data.Value;
import sisc.util.Util;

/**
 * This class serves only as an example, and should never be subclassed
 * Remove the abstract modifier when copying/pasting.
 *  
 * @author scgmille
 *
 */
public abstract class NativeModuleTemplate extends Util {

    // Ids for the indexed procedures
    // Next: 0
    public static final int IDS=-1;
    
    /**
     * The Simple procedures are purely functional procedures
     * which do not need to access interpreter registers to execute
     */
    public static class Simple extends IndexedFixableProcedure {
        public Simple() {}

        Simple(int id) {
            super(id);
        }
    }
    
    /**
     * The Complex procedures either have a side effect, or
     * require the interpreter to execute
     */
    public static class Complex extends CommonIndexedProcedure {
        public Complex() {}
        
        Complex(int id) {
            super(id);
        }
    }
    
    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {

        public Index() {
            //Definitions go here
        }
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
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
