package sisc.ser;

import java.io.IOException;
import java.io.ObjectInput;
import java.math.BigDecimal;
import java.math.BigInteger;

import sisc.data.Expression;
import sisc.data.Value;
import sisc.interpreter.AppContext;

public abstract class DeserializerImpl extends BerEncoding implements Deserializer {

    protected AppContext ctx;
    protected ObjectInput datin;

    protected DeserializerImpl(AppContext ctx, ObjectInput in) {
        this.ctx = ctx;
        datin = in;
    }

    public Expression resolveLibraryBinding(LibraryBinding lb)
        throws IOException {

        return ctx.resolveBinding(lb);
    }

    public BigInteger readBigInteger() throws IOException {
        byte[] buffer=new byte[readInt()];
        readFully(buffer);
        return new BigInteger(buffer);
    }

    public BigDecimal readBigDecimal() throws IOException {
        byte[] buffer=new byte[readInt()];
        int scale=readInt();
        readFully(buffer);
        return new BigDecimal(new BigInteger(buffer), scale);
    }

    public int readUnsignedByte() throws IOException {
        return readByte() & 0xff;
    }

    public int readUnsignedShort() throws IOException {
        return readShort() & 0xffff;
    }

    public boolean readBoolean() throws IOException {
        return datin.readBoolean();
    }

    public byte readByte() throws IOException {
        return (byte)readBer(datin);
    }

    public char readChar() throws IOException {
        return datin.readChar();
    }

    public double readDouble() throws IOException {
        return Double.longBitsToDouble(readLong());
    }

    public float readFloat() throws IOException {
        return Float.intBitsToFloat(readInt());
    }

    public int readInt() throws IOException {
        return readBer(datin);
    }

    public long readLong() throws IOException {
        return readBerLong(datin);
    }

    public short readShort() throws IOException {
        return readBerShort(datin);
    }

    public String readUTF() throws IOException {
        return datin.readUTF();
    }

    public void readFully(byte[] b) throws IOException {
        datin.readFully(b);
    }

    public void readFully(byte[] b, int offset, int len) throws IOException {
        datin.readFully(b, offset, len);
    }

    public int skipBytes(int bc) throws IOException {
        return datin.skipBytes(bc);
    }

    public int read(byte[] b) throws IOException {
        return datin.read(b);
    }

    public int read(byte[] b, int off, int len) throws IOException {
        return datin.read(b, off, len);
    }

    public int read() throws IOException {
        return datin.read();
    }

    public String readLine() throws IOException {
        return datin.readLine();
    }

    public Object readObject() throws IOException, ClassNotFoundException {
        return datin.readObject();
    }

    public long skip(long n) throws IOException {
        return datin.skip(n);
    }

    public int available() throws IOException {
        return datin.available();
    }

    public void close() throws IOException {
        datin.close();
    }

    public Value[] readValueArray() throws IOException {
        int l=readInt();
        Value[] v=new Value[l];
        readExpressionArray(v);
        return v;
    }
    
    public Expression[] readExpressionArray() throws IOException {
        int l=readInt();
        Expression[] v=new Expression[l];
        readExpressionArray(v);
        return v;
    }
    
    void readExpressionArray(Expression[] target) throws IOException {        
        for (int i=0; i<target.length; i++) {
            target[i]=readExpression();
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
