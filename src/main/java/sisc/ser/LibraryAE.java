package sisc.ser;

import java.io.IOException;
import java.util.*;
import sisc.data.*;
import sisc.env.MemorySymEnv;
import sisc.env.SymbolicEnvironment;
import sisc.util.ExpressionVisitor;

/**
 * An SymEnv backed by a random-access library. It can operate in two
 * modes:
 * 1) "observe" - keep track of bindings from which to later create a
 * library
 * 2) "retrieve" - access bindings in a library
 */
public class LibraryAE extends MemorySymEnv {

    static class LibraryBinding {
        public Library lib;
        public int entryPoint;

        public LibraryBinding(Library lib, int ep) {
            this.lib=lib;
            entryPoint=ep;
        }
    }

    protected LibraryBuilder lb;
    protected Library base;
    protected Map addressMap;
    protected Set bindWatch;
    protected int parentIdx=-1;

    /**
     * Operate in "observe" mode.
     *
     * @param name the name of the SymEnv
     * @param lb the library serializer
     */
    public LibraryAE(Symbol name, LibraryBuilder lb) {
        super(name);
        this.lb=lb;
        bindWatch=new HashSet();
    }


    /**
     * Operate in "observe" mode.
     *
     * @param parent the name of the parent SymEnv
     * @param lb the library serializer
     */
    public LibraryAE(SymbolicEnvironment parent, LibraryBuilder lb) {
        this.parent=parent;
        this.lb=lb;
        bindWatch=new HashSet();
    }

    
    /**
     * Operate in "retrieve" mode.
     *
     * @param base the library from which to retrieve bindings
     */
    public LibraryAE(Library base) {
        this.base=base;
        addressMap=new HashMap();
    }

    public SymbolicEnvironment getParent() {
        if (parent == null && parentIdx > -1)
           loadParent();
        return parent;
    }
     
    public void addSymbolicBindings(Library lib, Pair s) {
    for (;s!=EMPTYLIST; s=(Pair)s.cdr()) {
       Symbol nsym=(Symbol)s.car();
       addBinding(lib, nsym, lib.getEntryPoint(nsym));
    }
    }

    public void addBinding(Library lib, Symbol sym, int ep) {
        addressMap.put(sym, new LibraryBinding(lib, ep));
    }

    public void undefine(Symbol s) {
        if (bindWatch != null) bindWatch.remove(s);
        super.undefine(s);
    }

    private void loadParent() {
        if (parent == null || parentIdx > -1) {
            try {
                parent=(SymbolicEnvironment)base.getExpression(parentIdx);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public int getLoc(Symbol s) {
        synchronized(symbolMap) {
            //already loaded?
            Integer i = (Integer)symbolMap.get(s);
            if (i!=null) return i.intValue();
            //present in this AE?
            
            if (addressMap != null) {
                LibraryBinding b=(LibraryBinding)addressMap.get(s);
                if (b!=null) {
                    try {
                        return store(s, 
                                     (Value)b.lib.getExpression(b.entryPoint));
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                loadParent();
            }
            //try parent
            if (parent == null) return -1;
            Value v = parent.lookup(s);
            if (v == null) return -1;
            return store(s, v);
        }
    }
    
    /**
     * Catch all occurences of sets and note them for when we serialize
     *
     * @param s the key being set
     * @param v the value associated with the key
     * @return index of binding
     */
    public int store(Symbol s, Value v) {
        if (bindWatch != null) bindWatch.add(s);
        return super.store(s, v);
    }

    public LibraryAE() {}

    public void deserialize(Deserializer d) throws IOException {
        setName((Symbol)d.readExpression());
        base=((LibraryDeserializer)d).getLibrary();
        int size=d.readInt();
        addressMap=new HashMap(size);
        for (int i=0; i<size; i++) {
            Symbol name=(Symbol)d.readExpression();
            addBinding(base, name, d.readInt());
        }
        parentIdx=d.readInt();
        deserializeSidecar(d);        
    }

    public void serialize(Serializer s) throws IOException {
        s.writeExpression(getName());
        if (base == null) {
            //serialize in "observe" mode
            if (getName()!=null)
                lb.add(getName(), this);

            s.writeInt(bindWatch.size());
            for (Iterator i=bindWatch.iterator(); i.hasNext();) {
                Symbol key=(Symbol)i.next();
                s.writeExpression(key);
                //add binding as new entry point to library
                int pos=lb.add(super.lookup(key));
                s.writeInt(pos);
            }
            s.writeInt(lb.add(parent == null ? null : parent.asValue()));
        } else {
            //serialize in "retrieve" mode
            s.writeInt(0);
            for (Iterator i=addressMap.keySet().iterator(); i.hasNext();) {
                Symbol key=(Symbol)i.next();
                LibraryBinding b=(LibraryBinding)addressMap.get(key);
                s.writeExpression(key);
                s.writeInt(b.entryPoint);
            }
            s.writeInt(parentIdx);
        }
        serializeSidecar(s);
    }

    public boolean visit(ExpressionVisitor v) {
        visitSidecar(v);
        if (!v.visit(parent)) return false;
        if (base == null) {
            if (getName()!=null && v==lb)
                lb.add(getName(), this);
            for (Iterator i=bindWatch.iterator(); i.hasNext();) {
                Symbol key=(Symbol)i.next();
                if (!v.visit(key)) return false;
                //add binding as new entry point to library
                if (v==lb)
                    lb.add(super.lookup(key));
            }
            if (v==lb)
                lb.add(parent == null ? null : parent.asValue());
        } else {
            //serialize in "retrieve" mode
            for (Iterator i=addressMap.keySet().iterator(); i.hasNext();) {
                Symbol key=(Symbol)i.next();
                if (!v.visit(key)) return false;
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
