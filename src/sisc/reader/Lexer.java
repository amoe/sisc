package sisc.reader;

import java.io.*;
import sisc.data.*;
import sisc.util.Util;
import sisc.util.Defaults;

public class Lexer implements Tokens {

    /* Strict R5RS Syntax Helper Functions */
    public static final boolean isIdentifierStart(char c) {
        return Character.isLetter(c) ||
            in(c, special_initials);
    }

    public static final boolean isIdentifierSubsequent(char c) {
        return isIdentifierStart(c) ||
            Character.isDigit(c) ||
            in(c, special_subsequents);
    }

    /* Other Syntax helper functions */
    public static final boolean isPrintable(char c) {
        return !((c < ' ')  || (c > '~') || in(c, unprintable_characters));
    }
    
    static final char
        STRING_CONST='"',
        COMMENT     =';',
        LIST_OPEN_ALT ='[',
        LIST_CLOSE_ALT =']',
        LIST_OPEN  ='(',
        LIST_CLOSE =')',
        SHARP      ='#',
        QUOTE      ='\'',
        BACKQUOTE  ='`',
        UNQUOTE    =',',
        UNQUOTE_SPLICING
        ='@',
        DOT        ='.',
        PIPE       ='|';
    
    public static final char[]
        special = new char[]
        {'\t', '\n', '\r', ' ', '"', '(', ')', ';', '[', ']'},
        sharp_special = new char[]
        {'\t', '\n', ' ', '"', '#', '(', ')', '=', '[', ']'},
        number_prefixes = new char[]
        {'+','-','.'},
        hex_number_prefixes = new char[]
        {'+','-','.','A','B','C','D','E','F','a','b','c','d','e','f'},
        reserved = new char[] 
        {'[', ']', '{', '|', '}'},
        special_and_reserved = new char[]
        {'\t', '\n', '\r', ' ', '"', '(', ')', ';', '[', ']', '{', '|', '}'},
        special_initials = new char[] 
        {'!','$','%','&','*','/',':','<','=','>','?','^','_','~'},
        special_subsequents = new char[] 
        {'+','-','.','@'},
        protected_literal_barrier = new char[] 
        {'|'},
        unprintable_characters = new char[] 
        {};
        

    public boolean strictR5RS = Defaults.STRICT_R5RS;
    public String sval;
    public Quantity nval;
    public Pair prval;

    public int readIgnoringWhitespace(PushbackReader is)
    throws IOException {
        char c=0;

        do {
            c=(char)readChar(is);
        } while (Character.isWhitespace(c));

        return c;
    }

    public int nextToken(PushbackReader is, int radix) throws IOException {
        int nt=_nextToken(is, radix);
        return nt;
    }

    public int _nextToken(PushbackReader is, int radix)
    throws IOException {

        synchronized(is) {

            int c=readIgnoringWhitespace(is);
            switch (c) {
            case LIST_OPEN:
            case LIST_OPEN_ALT:
                return TT_PAIR;
            case LIST_CLOSE:
            case LIST_CLOSE_ALT:
                return TT_ENDPAIR;
            case PIPE:
                return TT_PIPE;
            case QUOTE:
                return TT_QUOTE;
            case SHARP:
                return TT_SHARP;
            case STRING_CONST:
                sval=readToEndOfString(is);
                return TT_STRING;
            case COMMENT:
                while (readChar(is,false,false,false)!='\n') {}
                return nextToken(is, radix);
            case BACKQUOTE:
                return TT_BACKQUOTE;
            case UNQUOTE:
                int sc=readChar(is);
                if (sc==UNQUOTE_SPLICING)
                    return TT_UNQUOTE_SPLICING;
                else
                    is.unread(sc);
                return TT_UNQUOTE;
            default:
                is.unread(c);
                String v=readToBreak(is, special, true, false);

                if (c=='\\') 
                   v="\\"+v;    

                Object result=v;
                if (numberStart(v.charAt(0), radix))
                    result=readNum(v, radix);
                if (result instanceof String) {
                    sval=(String)result;
                    if (sval.length()==1 &&
                            sval.charAt(0)==DOT)
                        return TT_DOT;
                    return TT_SYMBOL;
                } else {
                    nval=(Quantity)result;
                    return TT_NUMBER;
                }
            }
        }
    }

    static Object readNum(String v, int radix) {
        try {
            Quantity q=Quantity.valueOf(v, radix);
            return q;
        } catch (NumberFormatException n) {
            return v;
        }
    }

    public int readChar(PushbackReader is) throws IOException {
        return readChar(is, true, false, true); 
    }

    public int readPureChar(PushbackReader is) throws IOException {
        return readChar(is, true, false, false); 
    }

    public int readChar(PushbackReader is, boolean handleEscapes, 
                        boolean invertEscaped, boolean respectReserved) 
        throws IOException {
        int c=is.read();
        if (c==-1) throw new EOFException();
        if (strictR5RS && respectReserved && in((char)c, reserved)) 
            throw new IOException(Util.liMessage(Util.SISCB, "reservedchar", 
                                                 new String(new char[] {
                                                     ((char)c)})));

        if (!handleEscapes || c!='\\') return c;

        int rv=CharUtil.escapeSequenceToChar(is);
        return (invertEscaped ? -rv : rv);
    }

    public String readToEndOfString(PushbackReader is)
    throws IOException {
        StringBuffer b=new StringBuffer();
        do {
            int x=readPureChar(is);
            if (x=='"') break;
            b.append((char)x);
        } while(true);
       return b.toString();
    }

    public String readToBreak(PushbackReader is, char[] stops, 
                              boolean handleEscapes, boolean ignoreEscapedBreaks)
    throws IOException {
        StringBuffer b=new StringBuffer();
        char c;
        try {
            do {            
                int x=readChar(is, handleEscapes, true, true);
                boolean escaped=(x < 0);
                if (escaped) 
                   //Escaped character
                    c=(char)-x;
                else
                    c=(char)x;
                if (in(c, stops) && !(ignoreEscapedBreaks && escaped))
                    break;
                b.append(c);
            } while (true);
            is.unread(c);
        } catch (EOFException e) {
        }
        return b.toString();
    }

    public static boolean numberStart(char c, int radix) {
        return Character.isDigit(c) ||
               in(c, (radix == 16 ? hex_number_prefixes :
                      number_prefixes));
    }

    public static boolean in(char c, char[] set) {
        for (int i=set.length-1; i>-1; i--)
            if (c>set[i]) return false;
            else if (c==set[i]) return true;
        return false;
    }

    public static boolean contains(String c, char[] set) {
        for (int i=0; i<c.length(); i++)
            if (in(c.charAt(i), set)) return true;
        return false;
    }
    public void skipMultilineComment(PushbackReader in) 
        throws IOException {
        boolean seenSharp=false, seenPipe=false;
        int depth=0;
        try {
            do {
                switch (readChar(in)) {
                case PIPE:
                    if (seenSharp) {
                        seenSharp=false;
                        depth++;
                    } else {
                        seenPipe=true;
                    }
                    break;
                case SHARP:
                    if (seenPipe) {
                        seenPipe=false;
                        depth--;
                    } else {
                        seenSharp=true;
                    }
                    break;
                default:
                    seenPipe=seenSharp=false;
                }
            } while (depth >= 0);
        } catch (EOFException e) {
            System.err.println(Util.warn("eofbeforeeoc"));
            throw e;
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
