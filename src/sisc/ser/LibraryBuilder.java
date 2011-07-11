package sisc.ser;

import java.util.*;
import java.io.*;
import sisc.data.*;
import sisc.env.SymbolicEnvironment;
import sisc.interpreter.AppContext;
import sisc.util.ExpressionVisitor;
import sisc.util.ExpressionVisitee;

/**
 * Keeps track of entry points - points where serialization begins.
 *
 */
public class LibraryBuilder extends BerEncoding implements ExpressionVisitor {

    AppContext ctx;
    boolean addAllowed=true;
    Set classes, seen, duplicates;
    int dupid=0;
    LinkedList entryPoints, newEntryPoints, serQueue;
    Map names; 
    boolean includeAEs;

    public LibraryBuilder() {
        this(null);
    }
    
    public LibraryBuilder(AppContext ctx) {
        this(ctx, true);
    }

    public LibraryBuilder(AppContext ctx, boolean iae) {
        this.ctx=ctx;
        includeAEs=iae;
        classes=new HashSet();
        seen=new HashSet();
        duplicates=new HashSet();
        entryPoints=new LinkedList();
        newEntryPoints=new LinkedList();
        names=new HashMap();
    }

    public void setAppContext(AppContext ctx) {
        this.ctx=ctx;
    }

    /**
     * Add an entry point.
     *
     * @param name the name of the entry point
     * @param val the value of the entry point
     * @return the index of the new (or existing) entry point
     */
    public int add(Symbol name, Expression val) {
        int epidx=add(val);
        names.put(name, new Integer(epidx));
        return epidx;
    }

    /**
     * Add a shared data structure
     *
     * @param val the shared data structure
     * @return the index of the new (or existing) shared data structure
     */
    public int add(Expression val) {
        if (val==null) return -1;
        int epidx=entryPoints.indexOf(val);
        if (epidx==-1) {
            int nepidx=newEntryPoints.indexOf(val);
            if (nepidx!=-1)
                epidx=entryPoints.size() + nepidx;
        }
        if (epidx==-1 && addAllowed) {
            epidx=entryPoints.size() + newEntryPoints.size();
            newEntryPoints.add(val);
        }

        return epidx;
    }

    public int get(Expression val) {
        return entryPoints.indexOf(val);
    }

    public Library buildLibrary(String name, 
                                OutputStream out) throws IOException {
        DataOutputStream datout=new DataOutputStream(out);

        //Pass 1
        System.err.println("Pass 1: Discovery...");

        serQueue=new LinkedList();

        //serialization may create new entry points, so we loop until
        //no new ones are added
        while (!newEntryPoints.isEmpty()) {
            entryPoints.addAll(newEntryPoints);
            serQueue.addAll(newEntryPoints);
            newEntryPoints.clear();
            while (!serQueue.isEmpty()) {
                Expression e=(Expression)serQueue.removeFirst();
                if (e!=null) {
                    if (seen.contains(e)) {
                        if (!entryPoints.contains(e) && !newEntryPoints.contains(e)) {
                            duplicates.add(e);
                        }
                    } else {
                        seen.add(e);
                        classes.add(e.getClass());
                        if (e instanceof SymbolicEnvironment) {
                            if (includeAEs || (e.getName()==null))
                                e.visit(this);
                        } else e.visit(this);                                
                        if (!(e instanceof Singleton)) e.visitAnnotations(this);
                    }
                }
            }
        }

        addAllowed=false;
        
        duplicates.removeAll(entryPoints);
        //Create the index table
        Expression[] epv=new Expression[entryPoints.size()+duplicates.size()];
        int x=0;
        for (Iterator i=entryPoints.iterator(); i.hasNext();) 
            epv[x++]=(Expression)i.next();

        for (Iterator i=duplicates.iterator(); i.hasNext();) {
            epv[x++]=(Expression)i.next();
        }

        //Pass 2
        System.err.println("Pass 2: Write data segment");
        File tempFile=File.createTempFile("heap","tmp");
        tempFile.deleteOnExit();
        OutputStream fos=new BufferedOutputStream(new FileOutputStream(tempFile));
        Vector classv=new Vector(classes);
        BlockSerializer ss=new BlockSerializer(ctx, fos, classv, epv);

        for (Iterator i=entryPoints.iterator(); i.hasNext();) {
            Expression exp=(Expression)i.next();
            ss.serialize(exp);//writeExpression(exp);
        }
        fos.flush();
        fos.close();
        
        //Pass 3
        System.err.println("Pass 3: Write index");
        datout.writeUTF(Library.LIBRARY_VERSION);
        datout.writeUTF(name);
        writeBer(classes.size(), datout);
        for (int i=0; i<classv.size(); i++) {
            datout.writeUTF(((Class)classv.elementAt(i)).getName());
        }

        int[] offsets=ss.getOffsets();
        int[] sizes=ss.getSizes();

        writeBer(offsets.length, datout);
        for (int i=0; i<offsets.length; i++) {
            writeBer(offsets[i], datout);
            writeBer(sizes[i], datout);
        };
        writeBer(names.size(), datout);
        for (Iterator i=names.keySet().iterator(); i.hasNext();) {
            Symbol s=(Symbol)i.next();
            datout.writeUTF(s.symval);
            writeBer(((Integer)names.get(s)).intValue(), datout);
        }
        
        //Pass 4
        System.err.println("Pass 4: Append data segment");
        BufferedInputStream in=new BufferedInputStream(new FileInputStream(tempFile));
        int rc=0;
        byte[] buffer=new byte[65536];
        while (-1!=(rc=in.read(buffer))) {
            if (rc>0)
                datout.write(buffer, 0, rc);
        }
        datout.flush();

        System.err.println(classes.size()+" classes");
        System.err.println(offsets.length+" entry points");
        return null;
    }

    /*---Serialization first pass functions---*/
    public boolean visit(ExpressionVisitee e) {
        if (e!=null) {
            serQueue.addFirst(e);
            if (includeAEs && e instanceof SymbolicEnvironment) {
                SymbolicEnvironment se=(SymbolicEnvironment)e;
                if (se.getName()==null)
                    add(se.asValue());
                else 
                    add(se.getName(),se.asValue());
            }
        }
        return true;
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
