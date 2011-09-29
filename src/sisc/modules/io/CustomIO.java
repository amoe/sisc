package sisc.modules.io;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.data.*;

import sisc.io.custom.CustomPort;

public class CustomIO extends IndexedFixableProcedure {

    protected static Symbol IOB =
        Symbol.intern("sisc.modules.io.Messages");

    protected static final int
    	PORTLOCAL=1, SETPORTLOCAL=2, CUSTOMPORTPROCS=3, CUSTOMPORTQ=4;

    public static class Index extends IndexedLibraryAdapter {
        
        public Value construct(Object context, int id) {
            return new CustomIO(id);
        }
        
       public Index() {
    	   define("custom-port?",           CUSTOMPORTQ);
    	   define("custom-port-procedures", CUSTOMPORTPROCS);
    	   define("port-local",             PORTLOCAL);
    	   define("set-port-local!",        SETPORTLOCAL);
        }   
    }
    
    public static final CustomPort customport(Value o) {
        if (o instanceof CustomPort) {
            return (CustomPort) o;
        } else {
            typeError(IOB, "custom-port", o);
            return null;
        }
    }
    
    public CustomIO(int id) {
        super(id);
    }
    
    public CustomIO() {}

    public Value apply(Value v1) throws ContinuationException {
    	switch(id) {
    	case PORTLOCAL:
    		return customport(v1).getPortLocal();
    	case CUSTOMPORTPROCS:
    		return customport(v1).getProxy().getProcs();
    	case CUSTOMPORTQ:
    		return SchemeBoolean.get(v1 instanceof CustomPort);
    	default:
    		throwArgSizeException();
    	}
    	return VOID;
    }

    public Value apply(Value v1, Value v2) throws ContinuationException {
    	switch(id) {
    	case SETPORTLOCAL:
    		customport(v1).setPortLocal(v2);
    		return VOID;
    	default:
    		throwArgSizeException();
    	}
    	return VOID;
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
