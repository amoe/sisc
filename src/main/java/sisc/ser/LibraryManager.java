package sisc.ser;

import java.net.URL;
import java.net.MalformedURLException;
import java.io.*;
import java.util.*;
import sisc.util.Util;
import sisc.data.Expression;
import sisc.data.Symbol;
import sisc.interpreter.AppContext;

public class LibraryManager extends Util {
    
    class LoadableLibrary {
        URL path;
        Library handle;

        public LoadableLibrary(URL path) {
            this.path=path;
        }

        public LoadableLibrary(Library l) {
            this.handle=l;
        }

        public void open() throws IOException {
            if (handle==null) {
                SeekableDataInputStream sis;
                if (path.getProtocol().equals("file")) {
                    sis=new SeekableDataInputStream(
                         new BufferedRandomAccessInputStream(
                          new File(new File(path.getPath()), 
                                   path.getFile()).getCanonicalPath(),
                          "r", 8, 2048));
                } else {
                    sis=new SeekableDataInputStream(new MemoryRandomAccessInputStream(path.openStream()));
                }
                try {
                    handle=Library.load(ctx, sis);
                } catch (ClassNotFoundException cnf) {
                    cnf.printStackTrace();
                }
            }
        }

        public Library getLibrary() throws IOException {
            open();
            return handle;
        }
    }
    
    protected Map loadedLibraries;
    AppContext ctx;
    
    public LibraryManager(AppContext ctx) {
        this.ctx=ctx;
        loadedLibraries=new HashMap();
    }

    public Expression getExpression(Symbol name) throws IOException {
        for (Iterator i=loadedLibraries.values().iterator(); i.hasNext();) {
            LoadableLibrary ll=(LoadableLibrary)i.next();
            try {
                return ll.getLibrary().getLocalExpression(name);
            } catch (FileNotFoundException fnf) {
            }
        }
        throw new FileNotFoundException(liMessage(SISCB,
                                                  "namedlibbindingnotanywhere",
                                                  name.toString()));
    }
    
    /**
     * Returns the reference to a binding in the active libraries, or null
     * if the provided expression isn't an entry point in any library.
     */
    public LibraryBinding lookupBinding(Expression e) throws IOException {
        for (Iterator i=loadedLibraries.entrySet().iterator(); i.hasNext();) {
            Map.Entry entry=(Map.Entry)i.next();
            String name=(String)entry.getKey();
            LoadableLibrary ll=(LoadableLibrary)entry.getValue();
            int eid=ll.getLibrary().reverseLookup(e);
            if (eid>-1) {
                return new LibraryBinding(name, eid);
            }            
        }
        return null;
    }

    public void addLibrary(Library l) {
        loadedLibraries.put(l.getName(), new LoadableLibrary(l));
    }


    public void addLibrary(String name, URL l) {
        loadedLibraries.put(name, new LoadableLibrary(l));
    }
        
    public boolean loadLibrary(String name) {
        //Should eventually utilize a load path
        try {
            URL u=new URL(name);
            addLibrary(name, u);
            return true;
        } catch (MalformedURLException mfu) {
            mfu.printStackTrace();
        } 
        return false;
    }

    /** 
     * Returns an expression from an external library
     */
    public Expression resolveBinding(LibraryBinding lb) throws IOException {
        LoadableLibrary ll=(LoadableLibrary)loadedLibraries.get(lb.name);
        return ll.getLibrary().getExpression(lb.epid);
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
