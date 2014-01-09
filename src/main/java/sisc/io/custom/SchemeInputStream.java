package sisc.io.custom;

import java.io.IOException;
import java.io.InputStream;

import sisc.data.Pair;
import sisc.data.Procedure;
import sisc.data.Quantity;
import sisc.data.Value;
import sisc.modules.io.Buffer;
import sisc.util.Util;

public class SchemeInputStream extends InputStream implements CustomPortProxy {

    Procedure read, readBlock, available, close;
    
    public SchemeInputStream(Procedure read, Procedure readBlock, Procedure available, Procedure close) {
        this.read=read;
        this.readBlock=readBlock;
        this.available=available;
        this.close=close;
    }

    public int read() throws IOException {
        return ((Quantity) IOUtils.bridge(read, getHost())).intValue();
    }
    
    public int read(byte[] b, int offset, int length) throws IOException {
        Value[] args = new Value[] {
            getHost(),
            new Buffer(b),
            Quantity.valueOf(offset),
            Quantity.valueOf(length)
        };
        return ((Quantity) (IOUtils.bridge(readBlock, args))).intValue();
    }
    
    public int available() throws IOException {
        return ((Quantity) IOUtils.bridge(available, getHost())).intValue();
    }

    public void close() throws IOException {
        IOUtils.bridge(close, getHost());
    }

    public Pair getProcs() {
    	return Util.list(read, readBlock, available, close);
    }

    Value host;

    public Value getHost() {
    	return host;
    }

    public void setHost(Value host) {
    	this.host = host;
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
