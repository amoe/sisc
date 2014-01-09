package sisc.ser;

import java.io.*;

/**
 * Class for efficient integer representation.  A BER encoded integer
 * is represented as a variable length sequence of bytes, in network-byte
 * order, with 7 bits of data, and the topmost bit representing a "continue"
 * flag.  If set, another byte follows.  This allows numbers in the range
 * 0-127 to be represented with one byte, 128-16384 in two bytes, etc.
 */
public abstract class BerEncoding {

    static final int BER_MASK=0x7f, BER_CONT=0x80;

    public static void writeBer(long v, DataOutput dos) throws IOException {
        byte[] b=new byte[10];
        int p=9;
        
        while (v!=0) {
            b[p--]=(byte)((v & BER_MASK) | BER_CONT);
            v>>>=7;
        }
        b[9]&=BER_MASK;
        
        if (p==9) p=8;
        dos.write(b, p+1, b.length-(p+1));
    }

    public static long readBerLong(DataInput in) throws IOException {
        int b=in.readUnsignedByte();
        long val=b & BER_MASK;
        while ((b & BER_CONT) != 0) {
            b=in.readUnsignedByte();
            val=(val<<7) + (b & BER_MASK);
        }
        return val;
    }

    /*---helper functions---*/
    public static short readBerShort(DataInput dis) throws IOException {
        return (short)BlockDeserializer.readBer(dis);
    }

    public static int readBer(DataInput dis) throws IOException {
        return (int)readBerLong(dis);
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
