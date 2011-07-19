package sisc.modules.io;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.data.*;

import java.io.IOException;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.net.URL;

public class BinaryIO extends IndexedProcedure {

    protected static Symbol BINARYB =
        Symbol.intern("sisc.modules.io.Messages");

    protected static final int
    	//Next: 16
        BLOCKREAD=1, BLOCKWRITE=2, MAKEBUFFER=3, BUFFERQ=4,
        BUFFERLENGTH=5, BUFFERREF=6, BUFFERSET=7, BUFFERCOPY=8,
        OPENBINARYINPUTFILE = 9, OPENBINARYOUTPUTFILE= 10,
        BINARYINPUTPORTQ = 11, BINARYOUTPUTPORTQ = 12, 
        BUFFERCOMPARE=13,
        OPENBUFFEREDBININPORT = 14,
        OPENBUFFEREDBINOUTPORT = 15;



    public static class Index extends IndexedLibraryAdapter {
        
        public Value construct(Object context, int id) {
            return new BinaryIO(id);
        }
        
       public Index() {
            define("read-block",    BLOCKREAD);
            define("write-block",   BLOCKWRITE);
            define("make-buffer",   MAKEBUFFER);
            define("buffer?",       BUFFERQ);
            define("buffer-compare", BUFFERCOMPARE);
            define("buffer-length", BUFFERLENGTH);
            define("buffer-ref",    BUFFERREF);
            define("buffer-set!",   BUFFERSET);
            define("buffer-copy!",  BUFFERCOPY);
            define("open-binary-input-file",  OPENBINARYINPUTFILE);
            define("open-binary-output-file", OPENBINARYOUTPUTFILE);
            define("open-buffered-binary-input-port", OPENBUFFEREDBININPORT);
            define("open-buffered-binary-output-port", OPENBUFFEREDBINOUTPORT);            
            define("binary-input-port?",  BINARYINPUTPORTQ);
            define("binary-output-port?", BINARYOUTPUTPORTQ);
        }   
    }

    private static SchemeBinaryInputPort openBinInFile(Interpreter f, URL url)
        throws ContinuationException {
        try {
            return new SchemeBinaryInputPort(new BufferedInputStream(IO.getURLInputStream(url))); 
        } catch (IOException e) {
            IO.throwIOException(f, liMessage(IO.IOB, "erroropening", 
                                             url.toString()), e);
        }
        return null;
    }
    
    private static SchemeBinaryOutputPort openBinOutFile(Interpreter f,  URL url)
        throws ContinuationException {
        try {
            OutputStream out=new BufferedOutputStream(IO.getURLOutputStream(url));
            return new SchemeBinaryOutputPort(out);
        } catch (IOException e) {
            IO.throwIOException(f, liMessage(IO.IOB, "erroropening",
                                             url.toString()), e);
        }
        return null;
    }

    public BinaryIO(int id) {
        super(id);
    }
    
    public BinaryIO() {}

    static final Buffer buffer(Value v) throws ContinuationException {
        try {
            return (Buffer)v;
        } catch (ClassCastException e) { typeError(BINARYB, "buffer", v); }
        return null;
    }

    private static void writeBlock(Interpreter f, 
            Buffer b, int offset, int count, SchemeBinaryOutputPort outport) throws ContinuationException {        
        try {
            outport.getOutputStream().write(b.buf, 0, count);
        } catch (IOException e) {
            error(f, liMessage(BINARYB, "errorwriting",
                               outport.toString(),
                               e.getMessage()));
        }
    }
    
    private static Value readBlock(Interpreter f, 
            Buffer b, int offset, int count, SchemeBinaryInputPort inport) throws ContinuationException {
        byte[] buf=b.buf;
        try {
            int rv=inport.getInputStream().read(buf, offset, 
                    Math.min(buf.length-offset, count));
            if (rv==-1) return EOF;
            else return Quantity.valueOf(rv);
        } catch (IOException e) {
            error(f, liMessage(BINARYB, "errorreading", 
                               inport.toString(),
                               e.getMessage()));
        }
        return VOID;
    }
    
    public Value doApply(Interpreter f) throws ContinuationException {
        switch (f.vlr.length) {
        case 1:
            switch (id) {
            case MAKEBUFFER:
                return new Buffer(num(f.vlr[0]).indexValue());
            case BUFFERLENGTH:
                return Quantity.valueOf(buffer(f.vlr[0]).buf.length);
            case BUFFERQ:
                return truth(f.vlr[0] instanceof Buffer);
            case OPENBINARYINPUTFILE:
                return openBinInFile(f, url(f.vlr[0]));
            case OPENBINARYOUTPUTFILE:
                return openBinOutFile(f, url(f.vlr[0]));
            case BINARYINPUTPORTQ:
                return truth(f.vlr[0] instanceof SchemeBinaryInputPort);
            case BINARYOUTPUTPORTQ:
                return truth(f.vlr[0] instanceof SchemeBinaryOutputPort);
            case OPENBUFFEREDBININPORT: 
            	return new SchemeBinaryInputPort(new PushbackInputStream(new BufferedInputStream(bininport(f.vlr[0]).getInputStream())));
            case OPENBUFFEREDBINOUTPORT: 
            	return new SchemeBinaryOutputPort(new BufferedOutputStream(binoutport(f.vlr[0]).getOutputStream()));
            default:
                throwArgSizeException();
            }
        case 2:
            switch (id) {
            case BUFFERCOMPARE:
                Buffer b1=buffer(f.vlr[0]);
                Buffer b2=buffer(f.vlr[1]);
                int rv=b1.buf.length-b2.buf.length;
                if (rv==0) {
                    for (int i=0; i<b1.buf.length; i++) {
                        rv=b1.buf[i]-b2.buf[i];
                        if (rv!=0) break;
                    }              
                }
                return Quantity.valueOf(rv);
            case MAKEBUFFER:
                return new Buffer(num(f.vlr[0]).indexValue(),
                                  (byte)num(f.vlr[1]).indexValue());
            case BUFFERREF:
                int index=num(f.vlr[1]).indexValue();
                try {
                    return Quantity.valueOf(buffer(f.vlr[0]).ref(index)&0xff);
                } catch (ArrayIndexOutOfBoundsException aib) {
                    throwPrimException(liMessage(SISCB, "indexoob", 
                                                 new Object[] {
                                                     new Integer(index),
                                                     f.vlr[0].synopsis()}));
                }
            case OPENBINARYOUTPUTFILE:
                return openBinOutFile(f, url(f.vlr[0]));
            case OPENBUFFEREDBININPORT: 
            	return new SchemeBinaryInputPort(new BufferedInputStream(bininport(f.vlr[0]).getInputStream(),
            			num(f.vlr[1]).indexValue()));
            case OPENBUFFEREDBINOUTPORT: 
            	return new SchemeBinaryOutputPort(new BufferedOutputStream(binoutport(f.vlr[0]).getOutputStream(),
            			num(f.vlr[1]).indexValue()));
            default:
                throwArgSizeException();
            }
        case 3:
            switch (id) {
            case BUFFERSET:
                int index=num(f.vlr[1]).indexValue();
                try {
                    buffer(f.vlr[0]).set(index,
                                         (byte)num(f.vlr[2]).indexValue());
                } catch (ArrayIndexOutOfBoundsException aib) {
                    throwPrimException(liMessage(SISCB, "indexoob", 
                                                 new Object[] {
                                                     new Integer(index),
                                                     f.vlr[0].synopsis()}));
                }
                return VOID;
            case BLOCKREAD:
                return readBlock(f,buffer(f.vlr[0]), num(f.vlr[1]).indexValue(),
                        num(f.vlr[2]).indexValue(), bininport(f.dynenv.getCurrentInPort()));
            case BLOCKWRITE:
                writeBlock(f,buffer(f.vlr[0]), num(f.vlr[1]).indexValue(),
                        num(f.vlr[2]).indexValue(), binoutport(f.dynenv.getCurrentOutPort()));
                return VOID;
            default:
                throwArgSizeException();
            }
        case 4:
            switch (id) {            
            case BLOCKREAD:
                return readBlock(f,buffer(f.vlr[0]), num(f.vlr[1]).indexValue(),
                        num(f.vlr[2]).indexValue(), bininport(f.vlr[3]));
            case BLOCKWRITE:
                writeBlock(f,buffer(f.vlr[0]), num(f.vlr[1]).indexValue(),
                        num(f.vlr[2]).indexValue(), binoutport(f.vlr[3]));
                return VOID;
            case BUFFERCOPY:
                byte[] sbuf=buffer(f.vlr[0]).buf;
                byte[] dbuf=buffer(f.vlr[2]).buf;

                int soff=num(f.vlr[1]).indexValue();
                int doff=num(f.vlr[3]).indexValue();
                int count=sbuf.length;

                try {
                    System.arraycopy(sbuf, soff, dbuf, doff, count);
                } catch (ArrayIndexOutOfBoundsException aib) {
                    throwPrimException(liMessage(BINARYB, "bufferoverrun", 
                                                 f.vlr[0].synopsis(), 
                                                 f.vlr[2].synopsis()));
                }
                return VOID;
            default:
                throwArgSizeException();
            }
        case 5:
            switch (id) {            
            case BUFFERCOPY:
                byte[] sbuf=buffer(f.vlr[0]).buf;
                byte[] dbuf=buffer(f.vlr[2]).buf;

                int soff=num(f.vlr[1]).indexValue();
                int doff=num(f.vlr[3]).indexValue();
                int count=num(f.vlr[4]).indexValue();

                try {
                    System.arraycopy(sbuf, soff, dbuf, doff, count);
                } catch (ArrayIndexOutOfBoundsException aib) {
                    throwPrimException(liMessage(BINARYB, "bufferoverrun", 
                                                 f.vlr[0].synopsis(), 
                                                 f.vlr[2].synopsis()));
                }
                return VOID;
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
