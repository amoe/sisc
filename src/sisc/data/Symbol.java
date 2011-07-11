package sisc.data;

import java.io.*;

import sisc.io.ValueWriter;
import sisc.reader.*;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class Symbol extends Value {

    public static Symbol getUnique(String str) {
        return new Symbol(str);
    }

    /**
     * Interns the symbol with the given name.
     * @return the value of the symbol, or null if it is not defined
     */
    public static Symbol intern(String str) {
        return MemoizedSymbol.intern(str);
    }

    /**
     * Retrieves the value of the symbol with the given name.
     * Equivalent to <code>get(str,false)</code>.
     * @return the value of the symbol, or null if it is not defined
     */
    public static Symbol get(String str) {
        return get(str, false);
    }

    /**
     * Retrieves the value of the symbol with the given name.
     * @param str the name of the symbol
     * @param caseSensitive true if the case of the symbol name
     * is to be respected
     * @return the value of the symbol, or null if it is not defined
     */
    public static Symbol get(String str, boolean caseSensitive) {
        return intern(caseSensitive ? str : str.toLowerCase());
    }

    public String symval;

    public Symbol(String symval) {
        this.symval=symval;
    }

    public Symbol normalize() {
        return Symbol.get(symval.toLowerCase());
    }

    public void display(ValueWriter w) throws IOException {
        w.append(symval);
    }

    private void slashify(ValueWriter w, boolean protectedLiteral) throws IOException {
        for (int i=0; i<symval.length(); i++) {
            char c=symval.charAt(i);
            if (protectedLiteral) {
                if (c=='|' || !Lexer.isPrintable(c)) 
                    w.append('\\').append(CharUtil.charToEscaped(c));
                else w.append(c);
            } else {
                if (!Lexer.isIdentifierSubsequent(c)) {
                    w.append('\\').append(CharUtil.charToEscaped(c));                     
                } else w.append(c);
            }                  
        }
    }

    public int valueHashCode() {
        return symval.hashCode();
    }

    public boolean valueEqual(Value v) {
        return super.valueEqual(v) ||
                  ((v instanceof Symbol) && ((Symbol)v).symval.equals(symval));
    }

    public void write(ValueWriter w) throws IOException {
        if ((w.caseSensitive() || symval.toLowerCase().equals(symval))
            && !Lexer.contains(symval, Lexer.special_and_reserved)
            && (Parser.isPeculiarIdentifier(symval)
                || (symval.length()>0
                    && Lexer.isIdentifierStart(symval.charAt(0)))))
            slashify(w, false);
        else {
            w.append('|');
            slashify(w, true);
            w.append('|');
        }
    }

    public Symbol() {}

    public void serialize(Serializer s) throws IOException {
        s.writeUTF(symval);
    }

    public void deserialize(Deserializer s) throws IOException {
        symval = s.readUTF();
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
