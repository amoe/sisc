package sisc.modules.io;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.util.Util;
import sisc.data.*;

import java.io.IOException;
import java.io.EOFException;
import java.io.OutputStream;

import sisc.io.*;

public class SerialIO extends IndexedProcedure {

    protected static Symbol BINARYB =
        Symbol.intern("sisc.modules.io.Messages");

    protected static final int
        DESERIALIZE=1, SERIALIZE=2,
        OPENSERIALINPUTFILE = 3, OPENSERIALOUTPUTFILE= 4,
        SERIALINPORTQ=5, SERIALOUTPORTQ=6;

    public static class Index extends IndexedLibraryAdapter {
        
        public Value construct(Object context, int id) {
            return new SerialIO(id);
        }
        
       public Index() {
            define("serialize",               SERIALIZE);
            define("deserialize",             DESERIALIZE);
            define("open-serial-input-port",  OPENSERIALINPUTFILE);
            define("open-serial-output-port", OPENSERIALOUTPUTFILE);
            define("serial-input-port?", SERIALINPORTQ);
            define("serial-output-port?", SERIALOUTPORTQ);
        }   
    }
    
    public static final SerialOutputStream soutport(Value o) {
        try {
            return (SerialOutputStream)binoutport(o).getOutputStream();
        } catch (ClassCastException e) { typeError(BINARYB, "soutput-port", o); }
        return null;
    }

    public static final SerialInputStream sinport(Value o) {
        try {
            return (SerialInputStream)bininport(o).getInputStream();
        } catch (ClassCastException e) { typeError(BINARYB, "sinput-port", o); }
        return null;
    }

    private static SchemeBinaryInputPort openSerInPort(Interpreter f, 
                                                  SchemeBinaryInputPort sip)
        throws ContinuationException {
        try {
            return new SchemeBinaryInputPort(new DeserializerStream(f.getCtx(), sip.getInputStream()));
        } catch (IOException e) {
            IO.throwIOException(f, liMessage(BINARYB, "erroropening"), e);
        }
        return null;
    }

    private static SchemeBinaryOutputPort openSerOutPort(Interpreter f, 
                                                 SchemeBinaryOutputPort sop,
                                                 boolean aflush) 
        throws ContinuationException {
        try {
            OutputStream out=sop.getOutputStream();
            if (aflush) out=new AutoflushOutputStream(out);
            SerializerStream sp=new SerializerStream(f.getCtx(), out);
            sp.flush();
            return new SchemeBinaryOutputPort(sp);
        } catch (IOException e) {
            IO.throwIOException(f, liMessage(BINARYB, "erroropening"), e);
        }
        return null;
    }

    public static Value readSer(Interpreter r, SerialInputStream p)
        throws ContinuationException {
        try {
            return p.readSer();
        } catch (EOFException e) {
            return EOF;
        } catch (IOException e) {
            IO.throwIOException(r, liMessage(IO.IOB, "errorreading",
                                             p.toString(),
                                             e.getMessage()), e);
        }
        return null; //Should never happen
    }

    public static Value writeSer(Interpreter r, SerialOutputStream p,
                                 Value v)
        throws ContinuationException {
        try {
            p.writeSer(v);
        } catch (EOFException e) {
            return EOF;
        } catch (IOException e) {
            IO.throwIOException(r, liMessage(IO.IOB, "errorwriting",
                                             p.toString(),
                                             Util.javaExceptionToString(e)), e);
        }
        return VOID;
    }

    public SerialIO(int id) {
        super(id);
    }
    
    public SerialIO() {}

    public Value doApply(Interpreter f) throws ContinuationException {
        switch (f.vlr.length) {
        case 1:
            switch (id) {
            case SERIALOUTPORTQ: 
                return truth(f.vlr[0] instanceof SchemeBinaryOutputPort &&
                        ((SchemeBinaryOutputPort)f.vlr[0]).getOutputStream() instanceof SerialOutputStream);
            case SERIALINPORTQ: 
                return truth(f.vlr[0] instanceof SchemeBinaryInputPort &&
                        ((SchemeBinaryInputPort)f.vlr[0]).getInputStream() instanceof SerialInputStream);
            case OPENSERIALINPUTFILE:
                return openSerInPort(f, bininport(f.vlr[0]));
            case OPENSERIALOUTPUTFILE:
                return openSerOutPort(f, binoutport(f.vlr[0]), 
                                      false);
            case DESERIALIZE:
                return readSer(f, sinport(f.vlr[0]));
            default:
                throwArgSizeException();
            }
        case 2:
            switch (id) {
            case OPENSERIALOUTPUTFILE:
                return openSerOutPort(f, binoutport(f.vlr[0]), 
                                      truth(f.vlr[1]));
            case SERIALIZE:
                return writeSer(f, soutport(f.vlr[1]), f.vlr[0]);
            default:
                throwArgSizeException();
            }
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
