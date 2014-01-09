package sisc.modules.io;

import java.io.IOException;
import sisc.data.Value;
import sisc.data.NamedValue;
import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class Buffer extends Value implements NamedValue {
    
    public byte[] buf;

    public Buffer(int size) {
        this(new byte[size]);
    }

    public Buffer(int size, byte b) {
        this(size);
        for (int i=0; i<buf.length; i++) 
            buf[i]=b;
    }

    public Buffer(byte[] b) {
        buf=b;
    }

    public final byte ref(int idx) {
        return buf[idx];
    }

    public final void set(int idx, byte val) {
        buf[idx]=val;
    }

    public void display(ValueWriter w) throws IOException {
        displayNamedOpaque(w, "buffer");
    }

    public Buffer() {}
    
    public void serialize(Serializer s) throws IOException {
        s.writeInt(buf.length);
        s.write(buf);
    }

    public void deserialize(Deserializer s) throws IOException {
        buf=new byte[s.readInt()];
        s.read(buf, 0, buf.length);
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

    
