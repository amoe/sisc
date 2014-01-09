package sisc.reader;

import java.io.*;
import sisc.util.Util;

import java.util.Hashtable;
import sisc.data.SchemeCharacter;

public abstract class CharUtil {

    private static final Hashtable humanReadables=new Hashtable(8);

    private static void register(char c, String name) {
        SchemeCharacter sc=new SchemeCharacter(c);
        humanReadables.put(sc, name);
        humanReadables.put(name, sc);
    }

    static {
        register(' ', "space");
        register('\u0008', "backspace");
        register('\u007f', "rubout");
        register('\u000c', "page");
        register('\t', "tab");
        register('\n', "newline");
        register('\r', "return");
        register((char)0, "nul");
    }

    /**
     * Converts a human readable character name into the SchemeCharacter it represents.
     * 
     * @param namedCharConstant The human readable name of the character, e.g. "tab"
     * @return The SchemeCharacter named, or null if no such character exists
     */
    public static SchemeCharacter namedConstToChar(String namedCharConstant) {
        return (SchemeCharacter)humanReadables.get(namedCharConstant);
    }

    /**
     * Retreives the human readable character name of the given SchemeCharacter 
     * 
     * @param c The SchemeCharacter to attempt to convert
     * @return The SchemeCharacter The human readable name of the character, e.g. "tab", or
     *         null if no human readable name exists
     */
    public static String charToNamedConst(SchemeCharacter c) {
        return (String)humanReadables.get(c);
    }
    
    
    /**
     * Converts an escaped character to its real equivalent
     * 
     * @param c The character following the escape char ('\')
     * @return The real character represented by the escaped input
     */
    public static int escapedToChar(char c) {
        //escaping rules are those defined by Java, except we don't
        //handle octal escapes.
        switch (c) {
        case '"': return c | 0x80000000; 
        case 'b': return '\b'; 
        case 't': return '\t'; 
        case 'n': return '\n'; 
        case 'f': return '\f'; 
        case 'r': return '\r'; 
        default: return c;
        }
    }
    
    public static int escapeSequenceToChar(PushbackReader is) throws IOException {
        int c=is.read();
        if (c=='u') {
            char[] hexChars=new char[4];
            for (int i=0; i<hexChars.length; i++) {
                int rc=is.read();
                if (rc==-1) throw new EOFException("End of file on hex-literal");
                hexChars[i]=(char)rc;
            }
            try {
                return Integer.parseInt(new String(hexChars), 16);
            } catch (NumberFormatException nfe) {
                throw new IOException(Util.liMessage(Util.SISCB,
                                                     "invalidcharconst"));
            }
        } else return escapedToChar((char)c);
    }

    public static String charToEscapedIfNecessary(char c) {
        switch (c) {
        case '\b': return "b";
        case '\t': return "t";
        case '\n': return "n";
        case '\f': return "f";
        case '\r': return "r";
        case '\\': return "\\";
        case '"': return "\"";
        default: 
            if (c < ' ' || c > '~') return "u"+charToHex(c);
            else return null;
        }
        
    }
    public static String charToEscaped(char c) {
        String escapedChar=charToEscapedIfNecessary(c);
        if (escapedChar==null) return new String(new char[] {c});
        else return escapedChar;
    }
    
    public static char octToChar(String oct) {
        return (char)Integer.parseInt(oct, 8);
    }

    public static char hexToChar(String hex) {
        return (char)Integer.parseInt(hex, 16);
    }
    
    public static String charToOct(char c) {
        return Util.justify(Integer.toOctalString(c),3,'0');
    }

    public static String charToHex(char c) {
        return Util.justify(Integer.toHexString(c),4,'0');
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
