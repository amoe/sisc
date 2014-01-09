package sisc.interpreter;

import java.io.*;
import java.net.URL;

import sisc.ser.*;
import sisc.data.*;

import java.security.AccessControlException;
import java.util.Properties;
import sisc.env.SymbolicEnvironment;
import sisc.util.Util;

/**
 * The AppContext is the root of all data in an instance
 * of SISC.  This encapsulates mainly the procedures and data
 * which are defined in the global environment.  It is also
 * <i>the</i> token used by most Java to Scheme operations.
 * <p>
 * Typically, an AppContext is created using the default constructor,
 * then initialized with a heap using utility methods in {@link sisc.REPL}.
 * 
 **/
public class AppContext extends Util {

    public SymbolicEnvironment symenv;
    public SymbolicEnvironment toplevel_env;

    private LibraryManager libraries;
    private Properties props;

    /**
     * Create a new, AppContext with default values,
     * the recommended constructor for normal usage.
     * 
     */
    public AppContext() {
        this(new Properties());
    }

    /**
     * Create a new AppContext, providing a set of properties explicitly.
     * 
     * @param props Properties which govern the
     * underlying Scheme environment.
     */
    public AppContext(Properties props) {
        this.props = props;
        libraries=new LibraryManager(this);
    }

    /**
     * Create a new AppContext, providing a custom global environment.
     * 
     * @param symenv the global environment
     */
    public AppContext(SymbolicEnvironment symenv) {
        this();
        this.symenv = symenv;
        try {
            toplevel_env=lookupContextEnv(TOPLEVEL);
        } catch (ArrayIndexOutOfBoundsException ue) {
            toplevel_env=symenv;
            symenv.define(TOPLEVEL, toplevel_env.asValue());
        }
    }

    public Expression getExpression(Symbol name) {
        try {
            return libraries.getExpression(name);
        } catch(java.io.IOException e) { return null; }
    }

    // Heapfile loading/saving
    public void loadEnv(SeekableDataInputStream i)
        throws IOException, ClassNotFoundException {

        Library s=Library.load(this, i);
        
        libraries.addLibrary(s);
        
        SymbolicEnvironment lsymenv=(SymbolicEnvironment)s.getExpression(SYMENV);
        try {
            symenv=lsymenv;
            try {
                toplevel_env=lookupContextEnv(TOPLEVEL);
            } catch (ArrayIndexOutOfBoundsException e) {
                e.printStackTrace();
                throw new IOException("Heap did not contain toplevel environment!");
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new IOException(e.getMessage());
        }
    }

    public void saveEnv(OutputStream o, LibraryBuilder lb)
        throws IOException {
        lb.add(SYMENV, symenv.asValue());
        lb.add(TOPLEVEL, toplevel_env.asValue());

        lb.buildLibrary("sisc", o);
    }

    public SymbolicEnvironment lookupContextEnv(Symbol s) {
        SymbolicEnvironment senv = (SymbolicEnvironment)symenv.lookup(s);
        if (senv == null)
            throw new ArrayIndexOutOfBoundsException();
        return senv;
    }

    public void defineContextEnv(Symbol s, SymbolicEnvironment env) {
        symenv.define(s, env.asValue());
    }

    public String getProperty(String name) {
        String res = props.getProperty(name);
        if (res != null) return res;
        try {
            res = System.getProperty(name);
        } catch (SecurityException e) {}
        return res;
    }

    public String getProperty(String name, String def) {
        String res = getProperty(name);
        return (res == null) ? def : res;
    }


    public Expression resolveBinding(LibraryBinding lb) throws IOException {
        return libraries.resolveBinding(lb);
    }

    public LibraryBinding lookupBinding(Expression e) throws IOException {
        return libraries.lookupBinding(e);
    }

    /**
     * Given a SeekableInputStream which
     * is attached to a SISC heap file, loads the heap into this
     * AppContext and initializes it.  Returns true on success,
     * false otherwise.
     */       
    public boolean addHeap(SeekableInputStream in)
        throws ClassNotFoundException {
    
        try {
            loadEnv(new SeekableDataInputStream(in));
        } catch (IOException e) {
            System.err.println("\n"+Util.liMessage(Util.SISCB, 
                                                   "errorloadingheap"));
            e.printStackTrace();
            return false;
        }

        Interpreter r=Context.enter(this);
        try {
            try {
                File[] roots=File.listRoots();
                SchemeString[] rootss=new SchemeString[roots.length];
                for (int i=0; i<roots.length; i++)
                    rootss[i]=new SchemeString(roots[i].getPath());
                r.define(Symbol.get("fs-roots"),
                         Util.valArrayToList(rootss, 0, rootss.length),
                         Util.SISC);
            } catch (java.security.AccessControlException ace) {}
            
            try {
                r.eval("(initialize)");
            } catch (SchemeException se) {
                System.err.println(Util.liMessage(Util.SISCB, "errorduringinitialize")+
                                   Util.simpleErrorToString((Pair)se.m));
            } catch (IOException e) {
                System.err.println(Util.liMessage(Util.SISCB, "errorduringinitialize")+
                                   e.getMessage());
            }
        } finally {        
            Context.exit();
        }
        return true;
    }

    /**
     * Attempts to find and load the default SISC heap into this
     * AppContext.
     *
     * @see #findHeap(URL)
     */
    public void addDefaultHeap() throws IOException {
        URL u=findHeap(null);
        if (u == null) {
            throw new RuntimeException(Util.liMessage(Util.SISCB,
                                                      "errorloadingheap"));
        }
        try {
            if (!addHeap(openHeap(u)))
                throw new RuntimeException(Util.liMessage(Util.SISCB,
                                                          "errorloadingheap"));
        } catch(ClassNotFoundException e) {
            throw new RuntimeException(Util.liMessage(Util.SISCB,
                                                      "errorloadingheap"));
        }
    }

    public static SeekableInputStream openHeap(URL u) throws IOException {
        //Handle files separately, as they can be efficiently used
        //on disk in random access fashion.
        if (u.getProtocol().equals("file")) {
            try {
                return new BufferedRandomAccessInputStream(new File(u.getPath()), "r",  1, 8192);
            } catch (AccessControlException ace) {
                //Must be an applet, we'll have to load it as a URL stream
            }
        } 
        return new MemoryRandomAccessInputStream(u.openStream());
    }

    /**
     * Locate a heap.
     *
     * If the heap cannot be located at the supplied location then an
     * attempt is made to find it as a resource <tt>"sisc.shp"</tt>.
     *
     * @param heapLocation The URL for the heap file. When
     * this is <tt>null</tt> it defaults to the value of the
     * <tt>sisc.heap</tt> system property and, if that is not present,
     * <tt>"sisc.shp"</tt>
     */
    public static URL findHeap(URL heapLocation) {
        if (heapLocation==null) {
            try {
                heapLocation = Util.makeURL(System.getProperty("sisc.heap"));
                } catch (SecurityException se) {}
            if (heapLocation == null) {
                heapLocation = Util.makeURL("sisc.shp");
            }
        }
        if (mightExist(heapLocation)) return heapLocation;
        Class anchor=null;
        try {
            anchor=Class.forName("sisc.boot.HeapAnchor");
        } catch (ClassNotFoundException cnf) {}
        if (anchor==null) anchor=AppContext.class;
        heapLocation = anchor.getResource("/sisc/boot/sisc.shp");
        if (mightExist(heapLocation)) return heapLocation;
        return null;
    }

    private static boolean mightExist(URL u) {
        if (u == null) return false;
        if (u.getProtocol().equals("file")) {
            try {
                return new File(u.getPath()).exists();
            } catch (AccessControlException ace) {
                // Running as an applet.  need to actually open the file to try this out:
                try {
                    u.openStream().close();
                    return true;
                } catch (Exception e) {
                    //Oops, guess not
                    return false;
                }
                
            }
        } else return true;
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
