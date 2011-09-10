package sisc.modules.io;

import java.io.*;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.reader.SourceReader;
import sisc.data.*;
import sisc.io.*;

public class StringIO extends IndexedFixableProcedure {

    static Symbol IOB =
        Symbol.intern("sisc.modules.io.Messages");

    protected static final int
        //NEXT = 29,       
        GETOUTPUTSTRING     = 0,
        OPENINPUTSTRING     = 1,
        OPENSOURCEINPUTSTRING = 2,
        OPENOUTPUTSTRING    = 3,
        STRINGINPORTQ       = 4,
        STRINGOUTPORTQ      = 5;
        

    public static class Index extends IndexedLibraryAdapter { 

        public Value construct(Object context, int id) {
            return new StringIO(id);  
        }
        
        public Index() {
            define("get-output-string"  , GETOUTPUTSTRING);
            define("open-input-string"  , OPENINPUTSTRING);
            define("open-output-string" , OPENOUTPUTSTRING);
            define("open-source-input-string", OPENSOURCEINPUTSTRING);
            define("string-input-port?" , STRINGINPORTQ);
            define("string-output-port?", STRINGOUTPORTQ);
        }
    }
    
    public StringIO(int id) {
     super(id);
    }
    
    public StringIO() {}

    public Value apply() throws ContinuationException {
        switch (id) {
        case OPENOUTPUTSTRING:
            return new SchemeCharacterOutputPort(new StringWriter());
        default:
            throwArgSizeException();
        }
        return VOID;
    }
    
    public Value apply(Value v1) throws ContinuationException {
        switch (id) {
        case STRINGINPORTQ:
            //FIXME: This doesn't really do it
            return truth((v1 instanceof SchemeCharacterInputPort) &&
                         (((SchemeCharacterInputPort)v1).getReader() instanceof StringReader));

        case STRINGOUTPORTQ:
            return truth((v1 instanceof SchemeCharacterOutputPort) &&
                         (((SchemeCharacterOutputPort)v1).getWriter() instanceof StringWriter));
        case GETOUTPUTSTRING:
            OutputPort port=outport(v1);
            if (!(port instanceof SchemeCharacterOutputPort) ||
                !(((SchemeCharacterOutputPort)port).getWriter() 
                  instanceof StringWriter))
                throwPrimException( liMessage(IOB, "outputnotastringport"));
            try {
                port.flush();
            } catch (IOException e) {}

            StringWriter sw=(StringWriter)((SchemeCharacterOutputPort)port).getWriter();
            SchemeString s=new SchemeString(sw.getBuffer().toString());
            sw.getBuffer().setLength(0);
            return s;
        case OPENINPUTSTRING:
            return new SchemeCharacterInputPort(new PushbackReader(new StringReader(SchemeString.asString(v1))));
        case OPENSOURCEINPUTSTRING:
            return new SchemeCharacterInputPort(new SourceReader(new StringReader(SchemeString.asString(v1)), "<string>"));
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
