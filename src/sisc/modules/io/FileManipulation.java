package sisc.modules.io;

import java.io.*;
import java.net.*;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.util.Util;
import sisc.data.*;

/**
 * Scheme functions for manipulating files and directories.
 */
public class FileManipulation extends Util {

    public static final File fileHandle(Value o) {
        String encoding = "UTF-8";    // always supported
        
        try {
            URL u=url(o);
            if (!"file".equals(u.getProtocol()))
                Procedure.throwPrimException(liMessage(IO.IOB, "notafileurl"));
            String path=URLDecoder.decode(u.getPath(), encoding);
            return new File(path);
        } catch (UnsupportedEncodingException use) {
            Procedure.throwPrimException(
                liMessage(IO.IOB, "unsupencoding", encoding)
            );
            return null;    // not reached
        }
    }

    /**
     * The Simple procedures are purely functional procedures
     * which do not need to access interpreter registers to execute
     */
    public static class Simple extends IndexedFixableProcedure {
        public Simple() {}

        Simple(int id) {
            super(id);
        }

        public Value apply(Value v1) throws ContinuationException {
            switch(id) {
            case DIRECTORYQ:
                return truth(fileHandle(v1).isDirectory());
            case FILEQ:
                return truth(fileHandle(v1).isFile());
            case HIDDENQ:
                return truth(fileHandle(v1).isHidden());
            case READABLE:
                return truth(fileHandle(v1).canRead());
            case WRITEABLE:
                return truth(fileHandle(v1).canWrite());
            case DIRLIST:
                Pair p=EMPTYLIST;
                String[] contents=fileHandle(v1).list();
                if (contents == null)
                    throwPrimException(liMessage(IO.IOB, "nosuchdirectory",
                                                 SchemeString.asString(v1)));
                for (int i=contents.length-1; i>=0; i--) 
                    p=new Pair(new SchemeString(contents[i]), p);
                return p;
            case LENGTH:
                return Quantity.valueOf(fileHandle(v1).length());
            case LASTMODIFIED:
                return Quantity.valueOf(fileHandle(v1).lastModified());
            case GETPARENTURL:
                try {
                    return new SchemeString(
                        fileHandle(v1).getParentFile().toURI().toURL().toString()
                    );
                } catch (MalformedURLException m) {
                    m.printStackTrace();
                }
                break;
            default:
                throwArgSizeException();
            }
            return VOID;
        }
    }
    
    /**
     * The Complex procedures either have a side effect, or
     * require the interpreter to execute
     */
    public static class Complex extends CommonIndexedProcedure {
        public Complex() {}
   
        Complex(int id) {
            super(id);
        }

        public Value apply(Value v1) throws ContinuationException {
            switch(id) {
            case SETREADONLY:
                return truth(fileHandle(v1).setReadOnly());
            case DELETE:
                return truth(fileHandle(v1).delete());
            case MAKEDIRECTORY:
                return truth(fileHandle(v1).mkdir());
            case MAKEDIRECTORIES:
                return truth(fileHandle(v1).mkdirs());
            default:
                throwArgSizeException();
            }
            return VOID;
        }

        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch(id) {
            case SETLASTMODIFIED:
                return truth(fileHandle(v1).setLastModified(((Quantity) v2).longValue()));
            case RENAME:
                return truth(fileHandle(v1).renameTo(fileHandle(v2)));
            default:
                throwArgSizeException();
            }
            return VOID;
        }
    }
    
    /**
     * The Index 
     */
    public static class Index extends IndexedLibraryAdapter {

        public Index() {
            define("file/hidden?", HIDDENQ);
            define("file/is-directory?", DIRECTORYQ);
            define("file/is-file?", FILEQ);
            define("file/is-readable?", READABLE);
            define("file/is-writeable?", WRITEABLE);
            define("directory/list", DIRLIST);
            define("file/last-modified", LASTMODIFIED);
            define("file/set-last-modified!", Complex.class, SETLASTMODIFIED);
            define("file/set-read-only!", Complex.class, SETREADONLY);
            define("file/length", LENGTH);
            define("file/rename!", Complex.class, RENAME);
            define("file/delete!", Complex.class, DELETE);
            define("_get-parent-url", GETPARENTURL);
            define("_make-directory!", Complex.class, MAKEDIRECTORY);
            define("_make-directories!", Complex.class, MAKEDIRECTORIES);
        }
        
        public Value construct(Object context, int id) {
            if (context == null || context==Simple.class) {
                return new Simple(id);
            } else return new Complex(id);
        }
        
    }

    protected static final int DIRECTORYQ = 1,
        FILEQ = 2,
        HIDDENQ = 3,
        DIRLIST = 6,
        LASTMODIFIED = 7,
        SETLASTMODIFIED = 8,
        SETREADONLY = 9,
        LENGTH = 10,
        GETPARENTURL = 11,
        MAKEDIRECTORY = 12,
        MAKEDIRECTORIES = 13,
        RENAME = 14,
        DELETE = 15,
        READABLE = 16,
        WRITEABLE = 17;
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
