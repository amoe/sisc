package sisc.ser;

import java.util.*;
import java.io.*;
import java.net.URL;
import sisc.data.*;
import sisc.interpreter.AppContext;
import sisc.util.Util;
import sisc.interpreter.Context;

public class Library extends Util {
    static final String LIBRARY_VERSION="SLL4";
    
    protected String name;
    protected BlockDeserializer lib;
    protected Map names;

    public static Library load(AppContext ctx, URL u) throws IOException, ClassNotFoundException {
        if (u.getProtocol().equalsIgnoreCase("file")) {
            String path=u.getPath();
            return load(ctx, new SeekableDataInputStream(new BufferedRandomAccessInputStream(path, "r")));
        } else {
            return load(ctx, new SeekableDataInputStream(new MemoryRandomAccessInputStream(u.openStream())));
        }
    }
       
    public static Library load(AppContext ctx, SeekableDataInputStream di) throws IOException, ClassNotFoundException {
        String libver=di.readUTF();
        if (!libver.equals(LIBRARY_VERSION))
            throw new IOException(liMessage(SISCB, "unsuplib"));

        String libname=di.readUTF();
        int classCount=BerEncoding.readBer(di);
        Map classes=new HashMap(classCount);
        for (int i=0; i<classCount; i++) {
            classes.put(new Integer(i), Class.forName(di.readUTF()));
        }

        int socount=BlockDeserializer.readBer(di);
        int[] sharedObjectOffsets=new int[socount];
        int[] sharedObjectSizes=new int[socount];
        
        HashMap names=new HashMap();

        for (int i=0; i<socount; i++) {
            sharedObjectOffsets[i]=BlockDeserializer.readBer(di);
            sharedObjectSizes[i]=BlockDeserializer.readBer(di);
        }
        int symtableLength=BlockDeserializer.readBer(di);
        for (int i=0; i<symtableLength; i++) {
            String s=di.readUTF();
            int ep=BlockDeserializer.readBer(di);
            names.put(Symbol.intern(s), new Integer(ep));
        }

        return new Library(libname, new BlockDeserializer(ctx, di, classes, sharedObjectOffsets, sharedObjectSizes), names);
    }
        
    public Library(String name, BlockDeserializer lib, Map names) {
        this.name=name;
        this.lib=lib;
        this.names=names;
        lib.setLibrary(this);
    }

    public int getEntryPoint(Symbol name) {
        Integer i=(Integer)names.get(name);
        if (i==null) return -1;
        else return i.intValue();
    }

    public Expression getLocalExpression(Symbol name) throws IOException {
        Integer i=(Integer)names.get(name);
        if (i==null) 
            throw new FileNotFoundException(liMessage(SISCB, 
                                                      "namedlibbindingnotfound", 
                                                      name.toString()));
        return getExpression(i.intValue());
    }

    public Expression getExpression(Symbol name) throws IOException {
        try {
            return getLocalExpression(name);
        } catch (FileNotFoundException fnf) {
            return Context.currentAppContext().getExpression(name);
        }
    }

    public Expression getExpression(int oid) throws IOException {
        if (oid==-1) 
            return null;
        Expression rv=lib.fetchShared(oid);
        return rv;
    }

    public String getName() {
        return name;
    }

    /**
     * Given an expression, return the entry point id of the expression if any, -1 otherwise.
     */
    public int reverseLookup(Expression e) {
        for (int i=lib.alreadyReadObjects.length-1; i>=0; i--) {
            if (lib.alreadyReadObjects[i]==e) {
                return i;
            }
        }
        return -1;
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
