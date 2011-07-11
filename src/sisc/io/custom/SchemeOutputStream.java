package sisc.io.custom;

import java.io.IOException;
import java.io.OutputStream;

import sisc.data.Pair;
import sisc.data.Procedure;
import sisc.data.Quantity;
import sisc.data.Value;
import sisc.modules.io.Buffer;
import sisc.util.Util;

public class SchemeOutputStream extends OutputStream implements CustomPortProxy {

    Procedure write, writeBlock, flush, close;
    
    public SchemeOutputStream(Procedure write, Procedure writeBlock, Procedure flush, Procedure close) {
        this.write=write;
        this.writeBlock=writeBlock;
        this.flush=flush;
        this.close=close;
    }

    Value host;

    public Value getHost() {
    	return host;
    }

    public void setHost(Value host) {
    	this.host = host;
    }

    public Pair getProcs() {
    	return Util.list(write, writeBlock, flush, close);
    }

    public void write(int c) throws IOException {
        IOUtils.bridge(write, new Value[] {getHost(), Quantity.valueOf(c)});        
    }
    
    public void write(byte[] b, int offset, int length) throws IOException {
        IOUtils.bridge(writeBlock, new Value[] { getHost(),
                new Buffer(b), Quantity.valueOf(offset), Quantity.valueOf(length)});         
    }
    
    public void flush() throws IOException {
        IOUtils.bridge(flush, getHost());
    }

    public void close() throws IOException {
        IOUtils.bridge(close, getHost());
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
