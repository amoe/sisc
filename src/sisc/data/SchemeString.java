package sisc.data;

import java.io.*;

import sisc.io.ValueWriter;
import sisc.reader.CharUtil;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class SchemeString extends Value {

    //trade off speed vs memory
    public static boolean compactRepresentation = false;

    private char[] data_c = null;
    private String data_s = null;

    public SchemeString() {}

    public SchemeString(String s) {
        data_s=s;
    }

    public SchemeString(char[] data) {
        data_c=data;
    }

    public String asString() {
        if (data_s == null) {
            data_s=new String(data_c);
            if (compactRepresentation) data_c = null;
        }
        return data_s;
    }

    private char[] asCharArray() {
        if (data_c == null) {
            data_c=data_s.toCharArray();
            if (compactRepresentation) data_s = null;
        }
        return data_c;
    }

    public int length() {
        return (data_s == null) ? data_c.length : data_s.length();
    }

    public char charAt(int index) {
        return (data_s == null) ? data_c[index] : data_s.charAt(index);
    }

    public boolean valueEqual(Value v) {
        if (v == this) return true;
        if (!(v instanceof SchemeString)) return false;
        SchemeString o=(SchemeString)v;

        if (data_c != null && o.data_c != null) {
            char[] oc=o.data_c;
            if (data_c.length!=oc.length) return false;
            for (int i=0; i<oc.length; i++) {
                if (data_c[i]!=oc[i]) return false;
            }
            return true;
        } else {
            return asString().equals(o.asString());
        }
    }

    public int valueHashCode() {
        return asString().hashCode();
    }

    public void appendTo(StringBuffer buf) {
        if (data_c != null) {
            buf.append(data_c);
        } else {
            buf.append(data_s);
        }
    }

    public SchemeString copy() {
        if (data_c != null) {
            char[] newstr=new char[data_c.length];
            System.arraycopy(data_c, 0, newstr, 0, data_c.length);
            return new SchemeString(newstr);
        } else {
            return new SchemeString(asString());
        }
    }

    public SchemeString substring(int from, int to) {
        if (data_s != null) {
            return new SchemeString(data_s.substring(from, to));
        } else {
            int len=to-from;
            char[] newstr=new char[len];
            System.arraycopy(data_c, from, newstr, 0, len);
            return new SchemeString(newstr);
        }
    }

    public void set(int k, char c) {
        asCharArray();
        data_c[k]=c;
        data_s = null;
    }

    public void set(String s) {
        data_s = s;
        data_c = null;
    }

    public void set(char[] ca) {
        data_c = ca;
        data_s = null;
    }

    public int readFromReader(Reader r, int off, int len)
        throws IOException {

        asCharArray();
        return r.read(data_c, off, len);
    }

    public void writeToWriter(Writer w, int off, int len)
        throws IOException {

        asCharArray();
        w.write(data_c, off, len);
    }

    public void display(ValueWriter w) throws IOException {
        w.append(asString());
    }

    public void write(ValueWriter w) throws IOException {
        w.append(toString());
    }

    public String toString() {
        StringBuffer b=new StringBuffer();
        b.append('"');
        int lastGood=0;
        asCharArray();
        for (int i=0; i<data_c.length; i++) {
            String escapeString=CharUtil.charToEscapedIfNecessary(data_c[i]);
            if (escapeString == null) continue;
            
            b.append(data_c, lastGood, i-lastGood);
            b.append('\\').append(escapeString);
            lastGood=i+1;
        }

        b.append(data_c, lastGood, data_c.length-lastGood);
        b.append('"');
        return b.toString();
    }

    public void serialize(Serializer s) throws IOException {
        s.writeUTF(asString());
    }

    public void deserialize(Deserializer s) throws IOException {
        data_s=s.readUTF();
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
