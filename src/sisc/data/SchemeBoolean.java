package sisc.data;

import java.io.*;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class SchemeBoolean extends Value implements Singleton {
    public static SchemeBoolean TRUE=new SchemeBoolean(true);
    public static SchemeBoolean FALSE=new SchemeBoolean(false);
    public static Boolean JTRUE=new Boolean(true), JFALSE=new Boolean(false);
    private boolean val;

    public static SchemeBoolean get(boolean b) {
        return b ? TRUE : FALSE;
    }

    /**
     * Returns false if v is SchemeBoolean.FALSE, and true otherwise.
     */
    public static boolean toBoolean(Value v) {
        return v != FALSE;
    }

    protected SchemeBoolean(boolean v) {
        val=v;
    }

    public void display(ValueWriter w) throws IOException {
        w.append(val ? "#t" : "#f");
    }

    public int hashCode() {
        return (val ? 0x01010101 : 0x10101010);
    }

    public boolean equals(Object o) {
        return ((o instanceof SchemeBoolean) && ((SchemeBoolean)o).val==val);
    }

    public SchemeBoolean() {}

    public void serialize(Serializer s) throws IOException {
        s.writeBoolean(val);
    }

    public void deserialize(Deserializer s) throws IOException {
        val = s.readBoolean();
    }

    public Value singletonValue() {
        return val ? TRUE : FALSE;
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
